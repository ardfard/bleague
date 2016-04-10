{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module BLeague.Models.Fixture where

import           Protolude

import           Data.Aeson                 (FromJSON, ToJSON (..), Value (..),
                                             toJSON)
import qualified Data.Text as T
import           GHC.Generics
import           Data.Time
import qualified          BLeague.Models.User as U
import           BLeague.Crud hiding (create)
import qualified BLeague.Crud as C
import           BLeague.Types
import qualified Database.RethinkDB         as R
import           Database.RethinkDB.NoClash hiding (Change, Null, Object, group,
                                             status, table, toJSON)


data Fixture = Fixture
  { id :: T.Text
  , home     :: T.Text
  , homeTeam :: T.Text
  , away     :: T.Text
  , awayTeam :: T.Text
  , dateTime :: UTCTime
  } deriving (Show, Eq, Generic)

instance FromJSON Fixture
instance ToJSON Fixture
instance FromDatum Fixture
instance ToDatum Fixture

currentTime :: MonadIO m => m UTCTime
currentTime = liftIO getCurrentTime

table = R.table "fixtures"

type Team = T.Text

create :: (U.User, Team) -> (U.User, Team) -> App Fixture
create (homeP, homeT)  (awayP, awayT) = do
  fixture <- Fixture "" (U.id homeP) homeT (U.id awayP) awayT <$> currentTime
  res <- runDb $ table # C.create fixture
  return fixture { id = generatedKey res}

listForUser :: U.User -> App [Fixture]
listForUser (U.User uid _ _ _ _) = 
  runDb $ table # R.filter (\fixture -> fixture!"home" R.== uid R.|| (fixture!"away") R.== uid)

update :: Fixture -> App ()
update fixture_ = C.docsSave table (id fixture_) fixture_
