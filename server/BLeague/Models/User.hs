{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BLeague.Models.User where

import           MyPrelude hiding (get, getAll)
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
import           Safe (headMay)

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

userTable = R.table "users"

listUser :: App [User]
listUser = runDb $ userTable # orderBy [asc "id"]

find :: Text -> App (Maybe User)
find id = runDb $ userTable # get (expr id)

findByUsername :: Text -> App (Maybe User)
findByUsername u = do
  us <- runDb $ userTable # getAll usernameIndex [expr u]
  return $ headMay us

save :: Text -> User -> App ()
save = docsSave userTable

remove :: Text -> App ()
remove id = runDb $ userTable # get (expr id) # delete

secure :: (Functor m) => m User -> m SecureUser
secure = fmap SecureUser

insert :: User -> App User
insert u = do
  r <- runDb $ userTable # create u
  let user = u { id = generatedKey r}
  return user

usernameIndexName = "username"
usernameIndex = Index usernameIndexName

init :: App ()
init = do
  initDb $ runDb $ tableCreate userTable
  initDb $ runDb $ userTable # indexCreate usernameIndexName (!expr usernameIndexName)
