{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

module BLeague.Types where

import           MyPrelude

import           Data.Aeson                 (ToJSON)
import           Data.Pool                  (Pool)
import           Database.RethinkDB         (RethinkDBHandle)

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

runApp :: AppConfig -> App a -> ExceptT ServantErr IO a
runApp config action = runReaderT (unApp action) config

--------------------------------------------------------

type Endpoint = Text

data AppEnvironment = Dev | Production deriving (Show, Generic, Read)
instance ToJSON AppEnvironment

data Env = Env {
  port        :: Int,
  envDb       :: (Text, Integer),
  -- mandrill :: Text,
  endpoint    :: Endpoint,
  environment :: AppEnvironment,
  authSecret  :: Text
} deriving (Show)

askEndpoint :: App Endpoint
askEndpoint = asks (endpoint . env)
