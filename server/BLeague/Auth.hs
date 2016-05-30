{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module BLeague.Auth where
import           MyPrelude

import           Data.Aeson                 (FromJSON, ToJSON (..))
import           Data.Time                  (getCurrentTime)
import           Data.Typeable
import           GHC.Generics

import qualified BLeague.Models.User        as User
import           BLeague.Types
import           Servant

authCheck :: AppConfig -> BasicAuthCheck User.User
authCheck cfg =
  BasicAuthCheck check
  where check authData = do
          eres <- runExceptT $ runApp cfg $ checkCredential authData
          case eres of
            Left _ -> return Unauthorized
            Right res -> return res

checkCredential :: BasicAuthData -> App (BasicAuthResult User.User)
checkCredential (BasicAuthData name pass) = do
  muser <- User.findByUsername (toS name)
  return $ maybe NoSuchUser
                 (\u -> if User.hashedPassword u == (toS pass) then Authorized u else BadPassword)
                 muser

data SignUpResult = SignUpSuccess
                 | SignUpFailure Text
                   deriving (Read, Show, Eq, Ord, Generic)

instance ToJSON SignUpResult

data LoginDetails = LoginDetails { username :: Text
                                 , password :: Text
                                 } deriving (Read, Show, Eq, Generic)
instance FromJSON LoginDetails

data LoginResult = LoginSuccess User.User
                 | LoginFailure Text
                   deriving (Show, Eq, Generic)

instance ToJSON LoginResult

type AuthAPI = "newuser" :> ReqBody '[JSON] NewUserDetails :> Post '[JSON]  SignUpResult
          :<|> "login" :> ReqBody '[JSON] LoginDetails :> Post '[JSON] LoginResult

authAPI :: Proxy AuthAPI
authAPI = Proxy

authServer :: ServerT AuthAPI App
authServer =  newuser :<|> login

-- instance ToSample Credentials where
--   toSamples _ = [("Sample Credentials", (Credentials "testuser" "testpassword"))]

data NewUserDetails = NewUserDetails { nudUsername :: Text
                                     , nudPassword :: Text
                                     } deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON NewUserDetails

-- instance ToSample NewUserDetails where
--   toSamples _ = [("Sample New User Details", (NewUserDetails "testuser" "testpassword" "test@example.com"))]

-- | Create a new user account and log the user in.
newuser :: NewUserDetails -> App SignUpResult
newuser (NewUserDetails u p) = do
  now <- liftIO getCurrentTime
  let user = User.User "" u p False now
  _ <-  User.insert user
  return SignUpSuccess

login :: LoginDetails -> App LoginResult
login (LoginDetails u pass) = do
  muser <- User.findByUsername u
  case muser of
    Just user -> if User.hashedPassword user == pass
                 then return $ LoginSuccess user
                 else return $ LoginFailure "password mismatch"
    Nothing -> return $ LoginFailure "user not found"
