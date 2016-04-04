{-# LANGUAGE OverloadedStrings #-}

module Main where
import           BLeague.Crud       (connectDbPool)
import           BLeague.Types
import           Data.Maybe         (fromMaybe)
import           Data.Text          (pack)
import           Network.URI
import           Control.Monad.Except (runExceptT)
import           System.Environment
import           Version
import           BLeague.Api
import           Servant (ServantErr)

readEnv :: Read a => String -> a -> IO a
readEnv name def = do
  mval <- lookupEnv name
  return $ fromMaybe def (read <$> mval)

defEnv :: String -> String -> IO String
defEnv name def = do
    mval <- lookupEnv name
    return $ fromMaybe def mval

readAllEnv :: IO Env
readAllEnv = do
  port <- readEnv "PORT" 3001
  environment <- readEnv "ENV" Dev
  endpoint <- defEnv "ENDPOINT" "http://localhost:3001"
  db <- lookupDb
  s <- defEnv "AUTH_SECRET" "not_a_secret"
  return $ Env port db (pack endpoint) environment (pack s)

lookupDb :: IO (String, Integer)
lookupDb = do
  mdbs <- lookupEnv "RETHINKDB_PORT_28015_TCP"
  let mdb = readEndpoint =<< mdbs
  return $ fromMaybe ("localhost", 28015) mdb

readEndpoint :: String -> Maybe (String, Integer)
readEndpoint u = do
  uri <- parseURI u
  auth <- uriAuthority uri
  return  (uriRegName auth, readPort $ uriPort auth)
 where
   readPort = read . drop 1

main :: IO ()
main = do
  putStrLn "Server started"
  config <- initConfig
  runApi config

runAction :: AppConfig -> App a -> IO (Either ServantErr a)
runAction  conf action = runExceptT $ runApp conf action

initConfig :: IO AppConfig
initConfig = do
  env <- readAllEnv
  print env
  p <- connectDbPool (envDb env)
  return $ AppConfig "bleague" (pack generatedVersion) env p
