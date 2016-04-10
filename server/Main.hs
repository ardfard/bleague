{-# LANGUAGE OverloadedStrings #-}

module Main where
import           MyPrelude

import           BLeague.Crud       (connectDbPool)
import           BLeague.Types
import           Network.URI
import           System.Environment (lookupEnv)
import           Prelude (String, read)
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
  (h,p) <- lookupDb
  s <- defEnv "AUTH_SECRET" "not_a_secret"
  return $ Env port (toS h, p) (toS endpoint) environment (toS s)

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
  putText "Server started"
  config <- initConfig
  runApi config

runAction :: AppConfig -> App a -> IO (Either ServantErr a)
runAction  conf action = runExceptT $ runApp conf action

initConfig :: IO AppConfig
initConfig = do
  env <- readAllEnv
  print env
  p <- connectDbPool (envDb env)
  return $ AppConfig "bleague" generatedVersion env p
