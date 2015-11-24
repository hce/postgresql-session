module Network.Wai.Session.PostgreSQL
    ( dbStore
    , WithPostgreSQLConn (..)
    , StoreSettings (..)
    ) where

import Control.Exception.Base
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Int (Int64)
import Data.Serialize (encode, decode, Serialize)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple
import Network.Wai.Session

import qualified Data.ByteString as B

data StoreSettings = StoreSettings
    { storeSettingsSessionTimeout :: Int64
    , storeSettingsKeyGen :: IO B.ByteString
    }

class WithPostgreSQLConn a where
    withPostgreSQLConn :: a -> (Connection -> IO b) -> IO b

instance WithPostgreSQLConn Connection where
    withPostgreSQLConn conn = bracket (return conn) (\_ -> return ())

qryCreateTable      = "CREATE TABLE session (id bigserial NOT NULL, session_key character varying NOT NULL, session_created_at bigint NOT NULL, session_last_access bigint NOT NULL, session_value bytea NOT NULL, CONSTRAINT session_pkey PRIMARY KEY (id), CONSTRAINT session_session_key_key UNIQUE (session_key)) WITH (OIDS=FALSE);"
qryCreateSession    = "INSERT INTO session (session_key, session_created_at, session_last_access, session_value) VALUES (?,?,?,?)"
qryUpdateSession    = "UPDATE session SET session_value=?,session_last_access=? WHERE session_key=?"
qryLookupSession    = "SELECT session_value FROM session WHERE session_key=? AND session_last_access>=?"
qryLookupSession'   = "UPDATE session SET session_last_access=? WHERE session_key=?"
qryLookupSession''  = "SELECT session_value FROM session WHERE session_key=?"

dbStore :: (WithPostgreSQLConn a, Serialize k, Eq k, Serialize v, MonadIO m) => a -> StoreSettings -> IO (SessionStore m k v)
dbStore pool stos = do
    withPostgreSQLConn pool $ \ conn -> do
        unerror $ execute_ conn qryCreateTable
    return $ dbStore' pool stos

dbStore' :: (WithPostgreSQLConn a, Serialize k, Eq k, Serialize v, MonadIO m) => a -> StoreSettings -> SessionStore m k v
dbStore' pool stos Nothing = do
    newKey <- storeSettingsKeyGen stos
    let map     = [] :: [(k, v)]
        map'    = "" -- encode map
    curtime <- round <$> liftIO getPOSIXTime
    withPostgreSQLConn pool $ \ conn ->
        void $ execute conn qryCreateSession (newKey, curtime :: Int64, curtime, map' :: B.ByteString)
    backend pool newKey map
dbStore' pool stos (Just key) = do
    let map     = [] :: [(k, v)]
        map'    = "\"\"" -- encode map
    curtime <- round <$> liftIO getPOSIXTime
    res <- withPostgreSQLConn pool $ \ conn ->
        query conn qryLookupSession (key, curtime - storeSettingsSessionTimeout stos) :: IO [Only B.ByteString]
    case res of
        [Only _]    -> backend pool key map
        _           -> dbStore' pool stos Nothing

backend :: (WithPostgreSQLConn a, Serialize k, Eq k, Serialize v, MonadIO m) => a -> B.ByteString -> [(k, v)] -> IO (Session m k v, IO B.ByteString)
backend pool key mappe = do
    return ( (
        (reader pool key mappe)
      , (writer pool key mappe) )
     , return key )

reader :: (WithPostgreSQLConn a, Serialize k, Eq k, Serialize v, MonadIO m) => a -> B.ByteString -> [(k, v)] -> k -> m (Maybe v)
reader pool key mappe k = do
    curtime <- round <$> liftIO getPOSIXTime
    res <- liftIO $ withPostgreSQLConn pool $ \conn -> do
        void $ execute conn qryLookupSession' (curtime :: Int64, key)
        query conn qryLookupSession'' (Only key)
    res' <- case res of
        [Only store']    -> case decode (fromBinary store') of
            Right store     -> return $ k `lookup` store
            Left error      -> return Nothing
        []              -> return Nothing
    return res'

writer :: (WithPostgreSQLConn a, Serialize k, Eq k, Serialize v, MonadIO m) => a -> B.ByteString -> [(k, v)] -> k -> v -> m ()
writer pool key mappe k v = do
    curtime <- round <$> liftIO getPOSIXTime
    [Only store] <- liftIO $ withPostgreSQLConn pool $ \conn ->
        query conn qryLookupSession'' (Only key)
    let store'      = case decode (fromBinary store) of
            Right s             -> s
            _                   -> []
        store''     = ((k,v):) . (filter ((/=k) . fst)) $ store'
        store'''    = encode store''
    liftIO $ withPostgreSQLConn pool $ \conn ->
        void $ execute conn qryUpdateSession (Binary store''', curtime :: Int64, key)


ignoreSqlError :: SqlError -> IO ()
ignoreSqlError _ = pure ()

unerror :: IO a -> IO ()
unerror action = void action `catch` ignoreSqlError
