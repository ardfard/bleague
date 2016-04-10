{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


module BLeague.Crud where

import           MyPrelude hiding (get)

import qualified Data.HashMap.Strict        as HM
import           Data.Pool
import           Database.RethinkDB.Datum   (resultToMaybe)
import           Database.RethinkDB.NoClash

import           BLeague.Types

datumToValue :: (FromDatum a) => Maybe Datum -> Maybe a
datumToValue a = case a of
  Just r -> resultToMaybe $ fromDatum r
  Nothing -> Nothing

generatedKey :: WriteResponse -> Text
generatedKey = fromMaybe "" . head . fromMaybe [""] . writeResponseGeneratedKeys

writeChangeHead :: WriteResponse -> Maybe Change
writeChangeHead rs = writeResponseChanges rs >>= headMay

writeChange :: (FromDatum a) => (Change -> Datum) -> WriteResponse -> Maybe a
writeChange v rs = case writeChangeHead rs of
  Just r -> case datumToValue . Just $ v r of
    Just r' -> Just r'
    Nothing -> Nothing
  Nothing -> Nothing

writeChangeNew :: (FromDatum a) => WriteResponse -> Maybe a
writeChangeNew = writeChange newVal

writeChangeOld :: (FromDatum a) => WriteResponse -> Maybe a
writeChangeOld = writeChange oldVal

stripId :: Datum -> Datum
stripId (Object o) = Object $ HM.delete "id" o
stripId x = x

create :: ToDatum a => a -> Table -> ReQL
create o = insert (stripId $ toDatum o)

toDatumNoId :: ToDatum a => a -> Datum
toDatumNoId = stripId . toDatum

class (MonadIO m) => RethinkIO m where
  connPool :: m (Pool RethinkDBHandle)

instance RethinkIO App where
  connPool = asks conn

runDb :: (Expr query, Result r, RethinkIO m) => query -> m r
runDb q = do
  pool <- connPool
  liftIO $ withResource pool $ \h -> run h q


initDb :: App (Either RethinkDBError Datum) -> App ()
initDb action = do
  r <- action
  print $ "[INIT] " <> case r of
    Left err -> errorMessage err
    Right d -> show d


runPool :: (Expr query, Result r) => Pool RethinkDBHandle -> query -> IO r
runPool p q = withResource p $ \h -> run h q

connectDb :: (Text,Integer) -> IO RethinkDBHandle
connectDb (host_,port_) = use bleagueDb <$> connect (toS host_) port_ Nothing

disconnectDb :: RethinkDBHandle -> IO ()
disconnectDb  = close

connectDbPool :: (Text, Integer) -> IO (Pool RethinkDBHandle)
connectDbPool (h, p) = createPool (connectDb (toS h, p)) disconnectDb 1 10 5


createDb :: App ()
createDb = initDb . runDb . dbCreate $ bleagueDbName

bleagueDb :: Database
bleagueDb = db bleagueDbName

bleagueDbName :: Text
bleagueDbName = "bleague"

docsList :: FromDatum a => Table -> App [a]
docsList t = runDb $ t # orderBy [asc "id"]

docsFind :: FromDatum a => Table -> Text -> App (Maybe a)
docsFind t id = runDb $ t # get (expr id)

docsInsert :: ToDatum a => Table -> a -> App Text
docsInsert t s = do
    r <- runDb $ t # create s
    return $ generatedKey r

docsSave :: ToDatum a => Table -> Text -> a -> App ()
docsSave t id s = runDb $ t # get (expr id) # replace (const (toDatum s))

docsRemove :: Table -> Text -> App ()
docsRemove t id = runDb $ t # get (expr id) # delete

