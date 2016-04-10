
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module BLeague.Models.Standing where

import           Protolude

import           Control.Monad.IO.Class     
import           Data.Aeson                 (FromJSON, ToJSON (..), Value (..),
                                             toJSON)
import qualified Data.Text as T
import           GHC.Generics
import           Data.Time
import qualified          BLeague.Models.User as User
import           BLeague.Crud hiding (create)
import qualified BLeague.Crud as C
import           BLeague.Types
import qualified Database.RethinkDB         as R
import           Database.RethinkDB.NoClash hiding (Change, Null, Object, group,
                                             status, table, toJSON)
import qualified Data.Map as M

type Team = T.Text
type TeamMember = T.Text

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


