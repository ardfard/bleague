{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BLeague.Models.Session (newSession, validSessionCookie, clearSessionCookie, init) where

import           BLeague.Crud
import           BLeague.Models.User        (User )
import qualified BLeague.Models.User as U
import           BLeague.Types
import           Data.Aeson
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Database.RethinkDB         as R
import           Database.RethinkDB.NoClash hiding (Change, Null, Object, group,
                                             status, table, toJSON)
import           GHC.Generics
import Data.Monoid ((<>))
import            Prelude hiding (init)
data Session = Session
  { id   :: Text
  , userId :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Session
instance ToJSON Session
instance FromDatum Session
instance ToDatum Session

table = R.table "sessions"
userIndexName = "user.id"
userIndex = Index userIndexName

newSession :: User -> App Text
newSession u =   do
  let session = Session "" (U.id u)
  res <- runDb $ table # create session
  return $ "sess=" <> generatedKey res

validSessionCookie :: AppConfig -> Text -> IO Bool
validSessionCookie cfg sessCookie = do
  r <- runPool (conn cfg) $ table # get (expr parseSessionCookie) :: IO (Maybe Session)
  print parseSessionCookie
  case r of
    Nothing -> return False
    Just _  -> return True
  where
    parseSessionCookie = T.drop 5 sessCookie -- drop the "sess=" prefix.  TODO this should be done with an actual parser.

clearSessionCookie :: Text -> App Bool
clearSessionCookie sessCookie = do
  resp <- runDb $ table # get (expr parseSessionCookie ) # delete
  return $ writeResponseDeleted  resp == 1
  where
    parseSessionCookie = T.drop 5 sessCookie

init :: App ()
init = initDb $ runDb $ tableCreate table
