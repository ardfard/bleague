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

-- type UsersAPI =
       -- ReqBody UserSignup :> Post (Headers CookieHeader SecureUser)
  -- :<|> Capture "id" Text :> Get SecureUser
  -- :<|> Capture "id" Text :> "matchs" :> Get [Match]
