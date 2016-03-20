{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

module BLeague.Types where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader
import           Control.Monad.Trans.Either

import           Data.Aeson                 (ToJSON)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Monoid
import           Data.Pool                  (Pool)
import           Data.Text                  (Text, unpack)
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import           Database.RethinkDB         (RethinkDBHandle)
import qualified Database.RethinkDB         as RethinkDB

import           GHC.Generics

import           Servant

data AppConfig = AppConfig {
  appName :: Text,
  version :: Text,
  env     :: Env,
  conn    :: Pool RethinkDBHandle
} deriving (Show)

newtype App a = App {
  unApp :: ReaderT AppConfig (ExceptT ServantErr IO) a
} deriving (Monad, Functor, Applicative, MonadReader AppConfig, MonadError ServantErr, MonadIO)

runApp :: AppConfig -> App a -> EitherT ServantErr IO a
runApp config action = do
    res <- liftIO $ runExceptT $ runReaderT (unApp action) config
    EitherT $ return res


--------------------------------------------------------

type Endpoint = Text

data AppEnvironment = Dev | Production deriving (Show, Generic, Read)
instance ToJSON AppEnvironment

data Env = Env {
  port        :: Int,
  envDb       :: (String, Integer),
  -- mandrill :: Text,
  endpoint    :: Endpoint,
  environment :: AppEnvironment,
  authSecret  :: Text
} deriving (Show)

askEndpoint :: App Endpoint
askEndpoint = asks (endpoint . env)
