{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module BLeague.Api where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Reader
import           Control.Monad.Trans.Either

import           Data.Aeson
import           Data.ByteString                   (ByteString)
import           Data.HashMap.Strict               (HashMap)
import           Data.Maybe                        (fromJust)
import           Data.Monoid
import           Data.Pool
import           Data.Proxy
import           Data.Text                         (Text, pack, toUpper, unpack)
import           Data.Text.Encoding                (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy                    as TL
import qualified Data.Text.Lazy.Encoding           as TLE

import           GHC.Generics

import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Handler.Warp          (Port, run)
import           Network.Wai.Middleware.AddHeaders
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Static

import           Servant

import           BLeague.Auth
import           BLeague.Crud
import qualified BLeague.Models.Session            as Session
import qualified BLeague.Models.User               as User
import           BLeague.Types

data UserSignup = UserSignup
  { username             :: Text
  , password             :: Text
  , passwordConfirmation :: Text
  } deriving (Show, Eq)

type UsersAPI =
  Capture "id" Text :> Get '[JSON] User.SecureUser

checkNotFound :: App (Maybe a) -> App a
checkNotFound action = do
  res <- action
  case res of
    Nothing -> throwError  err404
    Just v -> return v

type MainAPI =
       AuthAPI
  :<|> AuthProtected :> "users" :> UsersAPI

apiServer :: ServerT MainAPI App
apiServer = authServer :<|> userGet
  where
    userGet id = checkNotFound $ User.secure <$> User.findByUsername id

server :: AppConfig -> Server MainAPI
server config = enter (Nat $ runApp config) apiServer

type RootAPI = MainAPI

rootServer :: AppConfig -> Server RootAPI
rootServer = server

api :: Proxy RootAPI
api = Proxy

data AppSettings = AppSettings
  { appName        :: Text
  , appVersion     :: Text
  , user           :: Maybe User.SecureUser
  , appEndpoint    :: Text
  , appEnvironment :: AppEnvironment
  } deriving (Show, Generic)

instance ToJSON AppSettings

stack :: Application -> Application
stack = cors'
  where
    cors' = cors (const $ Just corsResourcePolicy)

runApi :: AppConfig -> IO ()
runApi config = do
  let p = port $ env config
  _ <- runExceptT $ runApp config $ do
    createDb
    User.init
    Session.init
  putStrLn "Starting..."
  let context = config :. EmptyContext
  run p $ stack $ serveWithContext api context (rootServer config)

-- Cors ---------------------------------------------------
corsResourcePolicy :: CorsResourcePolicy
corsResourcePolicy = CorsResourcePolicy
  { corsOrigins = Nothing
  , corsMethods = ["GET", "HEAD", "OPTIONS", "POST", "PUT", "DELETE"]
  , corsRequestHeaders = simpleResponseHeaders <> ["Authorization"]
  , corsExposedHeaders = Nothing
  , corsMaxAge = Nothing
  , corsVaryOrigin = False
  , corsRequireOrigin = False
  , corsIgnoreFailures = False
  }
