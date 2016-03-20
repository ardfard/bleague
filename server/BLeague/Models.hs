{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BLeague.Models where

import           Control.Applicative
import           Data.Aeson                 (FromJSON, ToJSON (..), Value (..),
                                             toJSON)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.List                  (group, sort, sortBy)
import qualified Data.Map                   as M
import           Data.Pool
import           Data.Text                  (Text, unpack)
import           Data.Time
import           Prelude                    hiding (id)

import           BLeague.Crud
import           BLeague.Types
import qualified Database.RethinkDB         as R
import           Database.RethinkDB.NoClash hiding (Change, Null, Object, group,
                                             status, table, toJSON)
import           GHC.Generics

--------------------------- User API -----------------------------
data User = User
  { id             :: Text
  , username       :: Text
  , hashedPassword :: Text
  , admin          :: Bool
  , created        :: UTCTime
  } deriving (Show, Eq, Generic)

newtype SecureUser = SecureUser User deriving (Show, Generic)

instance ToJSON SecureUser where
  toJSON (SecureUser user) = Object $ foldr HashMap.delete obj ["hashedPassword"]
    where (Object obj) = toJSON user

instance FromJSON User
instance ToJSON User
instance FromDatum User
instance ToDatum User

data Fixture = Fixture
  { dateTime :: UTCTime
  , home     :: User
  , away     :: User
  } deriving (Show, Eq, Generic)

instance FromJSON Fixture
instance ToJSON Fixture

type Team = Text

data League = League
  { title       :: Text
  , description :: Text
  , standings   :: M.Map User Standing
  } deriving (Show, Eq, Generic)

type TeamMember = Text

data Standing = Standing
  { team        :: Team
  , win         :: Int
  , draw        :: Int
  , lose        :: Int
  , goalFor     :: Int
  , goalAgainst :: Int
  , scorers     :: M.Map TeamMember Int
  , savers      :: M.Map TeamMember Int
  } deriving (Show, Eq, Generic)

instance FromJSON Standing
instance ToJSON Standing

userTable = R.table "users"

listUser :: App [User]
listUser = runDb $ userTable # orderBy [asc "id"]

findUser :: Text -> App (Maybe User)
findUser id = runDb $ userTable # get (expr id)

saveUser :: Text -> User -> App ()
saveUser = docsSave userTable

removeUser :: Text -> App ()
removeUser id = runDb $ userTable # get (expr id) # delete

secure :: (Functor m) => m User -> m SecureUser
secure = fmap SecureUser

insertUser :: User -> App User
insertUser u = do
  r <- runDb $ userTable # create u
  let user = u { id = generatedKey r}
  return user

init :: App ()
init = initDb $ runDb $ tableCreate userTable

