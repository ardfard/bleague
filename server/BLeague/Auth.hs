{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BLeague.Auth where

import           Data.Aeson                 (FromJSON, ToJSON (..))
import qualified Data.Aeson                 as Aeson
import           Data.ByteString            hiding (head, last, pack)
import           Data.Map.Lazy              (fromList)
import qualified Data.Map.Lazy              as Map
import           Data.Maybe                 (fromJust, fromMaybe, isJust)
import           Data.Monoid                ((<>))
import           Data.Pool                  (Pool)
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Text                  (Text, pack, toLower)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Data.Time
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LC8
import           Database.RethinkDB.NoClash (RethinkDBHandle)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask)
import           Control.Monad.Except (throwError, ExceptT(..))
import           GHC.Generics
import           Network.HTTP.Types         hiding (Header)
import           Network.Wai
import           Network.Wai.Handler.Warp   (run)

import           BLeague.Models.Session
import qualified BLeague.Models.User as User
import           BLeague.Types
import           BLeague.Crud
import           Servant.Server.Internal
import           Servant
import           Servant.Docs
-- | Type combinator to protect specified APIs from users who do not
-- have valid sessions.
--
-- To use:
--
-- > type SecretAPI = AuthProtected :> ("name" :> Get '[JSON] String
-- >                                :<|> "age"  :> Get '[JSON] Int )
-- > type MyAPI = "public" :> Get '[JSON] String
-- >         :<|> "auth" :> authAPI
-- >         :<|> "protected" :> SecretAPI
--
-- Note that the protected api will have a longer path (e.g., "protected/name")
--
-- Then create the server as you normally would:
--
-- > secretServer :: Server SecretAPI
-- > secretServer = getName :<|> getAge
-- >
-- > server :: Server MyAPI
-- > server = public
-- >     :<|> authServer
-- >     :<|> secretServer
--
type DBLookup = AppConfig -> Text -> IO Bool


isGoodCookie :: DBLookup
isGoodCookie config sessionCookie = do
  res <- validSessionCookie config sessionCookie
  return res

data AuthProtected

instance (HasContextEntry context (AppConfig), HasServer rest context) => HasServer (AuthProtected :> rest) context where
  type ServerT (AuthProtected :> rest) m = ServerT rest m

  route Proxy context subserver = WithRequest $ \ request ->
    route (Proxy :: Proxy rest) context $ addAcceptCheck subserver $ cookieCheck request
      where
        cookieCheck req = case lookup "Cookie" (requestHeaders req) of
            Nothing -> do
              return $ FailFatal err401 { errBody = "Missing auth header" }
            Just v  -> do
              let pool = getContextEntry context
              authGranted <-  isGoodCookie pool $ decodeUtf8 v
              if authGranted
                then return $ Route ()
                else return $ FailFatal err403 { errBody = "Invalid cookie" }

data LoginResult = LoginSuccess
                 | LoginFailure String
                   deriving (Read, Show, Eq, Ord, Generic)

instance ToJSON LoginResult

type AuthAPI = "login" :>  ReqBody '[JSON] Credentials :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] LoginResult)
           :<|> "newuser" :> ReqBody '[JSON] NewUserDetails :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] LoginResult)
           :<|> "logout" :> Header "Cookie" Text :> Get '[JSON] Bool
           :<|> "loggedin" :> Header "Cookie" Text :> Get '[JSON] Bool

authAPI :: Proxy AuthAPI
authAPI = Proxy

authServer :: ServerT AuthAPI App
authServer = login
        :<|> newuser
        :<|> logout
        :<|> loggedIn

instance ToCapture (Header "Cookie" Text) where
  toCapture _ =
    DocCapture "SessionCookie"          -- name
               "Session cookie string." -- description

data Credentials = Credentials { username :: Text
                               , password :: Text
                               } deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON Credentials
instance FromFormUrlEncoded Credentials where
  fromFormUrlEncoded theMap = do
    username <- lookupEither "Could not find username" "username" theMap
    password <- lookupEither "Could not find password" "password" theMap
    return Credentials {..}

-- | Wrapper around `lookup` that returns an either, with a provided failure message.
lookupEither :: Eq a => String -> a -> [(a, b)] -> Either String b
lookupEither err key map = case lookup key map of
  Nothing -> Left err
  Just  v -> Right v

-- instance ToSample Credentials where
--   toSamples _ = [("Sample Credentials", (Credentials "testuser" "testpassword"))]

data NewUserDetails = NewUserDetails { nudUsername :: Text
                                     , nudPassword :: Text
                                     } deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON NewUserDetails
instance FromFormUrlEncoded NewUserDetails where
  fromFormUrlEncoded theMap = do
    nudUsername <- lookupEither "Could not find username" "username" theMap
    nudPassword <- lookupEither "Could not find password" "password" theMap
    return NewUserDetails {..}

-- instance ToSample NewUserDetails where
--   toSamples _ = [("Sample New User Details", (NewUserDetails "testuser" "testpassword" "test@example.com"))]

login :: Credentials -> App (Headers '[Header "Set-Cookie" Text] LoginResult)
login cr = do
  euser <- checkCredential cr
  case euser of
    Right user -> doLogin user
    Left err -> throwError $ err403 {errBody = err}
  where
    checkCredential (Credentials name pass) = do
      muser <- User.findByUsername name
      return $ maybe (Left "User Invalid")
                     (\u -> if User.hashedPassword u == pass then Right u else Left "Password Invalid")
                     muser

doLogin :: User.User -> App (Headers '[Header "Set-Cookie" Text] LoginResult)
doLogin user = do
     sessionCookie <- newSession user
     return $ addHeader sessionCookie LoginSuccess

loggedIn :: Maybe Text -> App Bool
loggedIn Nothing              = return False
loggedIn (Just sessionCookie) = do
  conf <- ask
  liftIO $ validSessionCookie conf sessionCookie

-- | Create a new user account and log the user in.
newuser :: NewUserDetails -> App (Headers '[Header "Set-Cookie" Text] LoginResult)
newuser (NewUserDetails u p) = do
  now <- liftIO getCurrentTime
  let user = User.User "" u p False now
  _ <-  User.insert user
  doLogin user

-- | Invalidate the current session cookie.
--
-- Returns True if you were logged out, False if you were not logged in in the first place.
logout :: Maybe Text -> App Bool
logout Nothing       = return False
logout (Just cookie) = clearSessionCookie cookie

