module Network.Wai.Session.PostgreSQL
    ( dbStore
    , defaultSettings
    , purgeOldSessions
    , purger
    , ratherSecureGen
    , WithPostgreSQLConn (..)
    , StoreSettings (..)
    ) where

import Control.Concurrent
import Control.Exception.Base
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Int (Int64)
import Data.Serialize (encode, decode, Serialize)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple
import Network.Wai.Session
import Numeric (showHex)
import System.Entropy (getEntropy)

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- |These settings control how the session store is behaving
data StoreSettings = StoreSettings {
    -- |The number of seconds a session is valid
    -- Seconds are counted since the session is last accessed (read or written),
    -- not since it was created.
      storeSettingsSessionTimeout :: Int64
    -- |A random session key generator. The session ID should provide
    -- sufficient entropy, and must not be predictable. It is recommended
    -- to use a cryptographically secure random number generator.
    , storeSettingsKeyGen :: IO B.ByteString
    -- |Whether to create the database table if it does not exist upon
    -- creating the session store. If set to false, the database table
    -- must exist or be created by some other means.
    , storeSettingsCreateTable :: Bool
    -- |A function that is called by to log events such as session
    -- purges or the table creation.
    , storeSettingsLog :: String -> IO ()
    -- |The number of microseconds to sleep between two runs of the
    -- old session purge worker.
    , storeSettingsPurgeInterval :: Int
    }

-- |By default, you pass a postgresql connection to the session store
-- when creating it. The passed connection will have to stay open
-- for the (possibly very long) existence of the session and it should
-- not be used for any other purpose during that time.
-- You can implement an instance of this class for a connection pool
-- instead, so that the session manager will not require a permanent
-- open PostgreSQL connection.
class WithPostgreSQLConn a where
    -- |Call the function (Connection -> IO b) with a valid and open
    -- PostgreSQL connection.
    withPostgreSQLConn :: a -> (Connection -> IO b) -> IO b

instance WithPostgreSQLConn Connection where
    withPostgreSQLConn conn = bracket (return conn) (\_ -> return ())

qryCreateTable      = "CREATE TABLE session (id bigserial NOT NULL, session_key character varying NOT NULL, session_created_at bigint NOT NULL, session_last_access bigint NOT NULL, session_value bytea NOT NULL, CONSTRAINT session_pkey PRIMARY KEY (id), CONSTRAINT session_session_key_key UNIQUE (session_key)) WITH (OIDS=FALSE);"
qryCreateSession    = "INSERT INTO session (session_key, session_created_at, session_last_access, session_value) VALUES (?,?,?,?)"
qryUpdateSession    = "UPDATE session SET session_value=?,session_last_access=? WHERE session_key=?"
qryLookupSession    = "SELECT session_value FROM session WHERE session_key=? AND session_last_access>=?"
qryLookupSession'   = "UPDATE session SET session_last_access=? WHERE session_key=?"
qryLookupSession''  = "SELECT session_value FROM session WHERE session_key=?"
qryPurgeOldSessions = "DELETE FROM session WHERE session_last_access<?"
qryUpdateKey        = "UPDATE session SET session_key=? WHERE session_key=?"

-- |Create a new postgresql backed wai session store.
dbStore :: (WithPostgreSQLConn a, Serialize k, Eq k, Serialize v, MonadIO m) => a -> StoreSettings -> IO (SessionStore m k v)
dbStore pool stos = do
    when (storeSettingsCreateTable stos) $
        withPostgreSQLConn pool $ \ conn ->
            unerror $ execute_ conn qryCreateTable
    return $ dbStore' pool stos

-- |Delete expired sessions from the database.
purgeOldSessions :: WithPostgreSQLConn a => a -> StoreSettings -> IO Int64
purgeOldSessions pool stos = do
    curtime <- round <$> liftIO getPOSIXTime
    count <- withPostgreSQLConn pool $ \ conn ->
        execute conn qryPurgeOldSessions (Only (curtime - storeSettingsSessionTimeout stos))
    storeSettingsLog stos $ "Purged " ++ show count ++ " session(s)."
    return count

-- |Run a thread using forkIO that runs periodically to
-- purge old sessions.
purger :: WithPostgreSQLConn a => a -> StoreSettings -> IO ThreadId
purger pool stos = forkIO . forever . unerror $ do
    purgeOldSessions pool stos
    threadDelay $ storeSettingsPurgeInterval stos

-- |Create default settings using a session timeout of
-- one hour, a cryptographically secure session id generator
-- using 24 bytes of entropy and putStrLn to log events
-- to stdout.
defaultSettings :: StoreSettings
defaultSettings = StoreSettings
    { storeSettingsSessionTimeout=3600
    , storeSettingsKeyGen=ratherSecureGen 24
    , storeSettingsCreateTable=True
    , storeSettingsLog=putStrLn
    , storeSettingsPurgeInterval=600000000
    }

-- |Generate a session ID with n bytes of entropy
ratherSecureGen :: Int -> IO B.ByteString
ratherSecureGen n = TE.encodeUtf8 . prettyPrint <$> getEntropy n

prettyPrint :: B.ByteString -> T.Text
prettyPrint = T.pack . concatMap (`showHex` "") . B.unpack

dbStore' :: (WithPostgreSQLConn a, Serialize k, Eq k, Serialize v, MonadIO m) => a -> StoreSettings -> SessionStore m k v
dbStore' pool stos Nothing = do
    newKey <- storeSettingsKeyGen stos
    let map     = [] :: [(k, v)]
        map'    = "" -- encode map
    curtime <- round <$> liftIO getPOSIXTime
    withPostgreSQLConn pool $ \ conn ->
        void $ execute conn qryCreateSession (newKey, curtime :: Int64, curtime, map' :: B.ByteString)
    backend pool stos newKey map
dbStore' pool stos (Just key) = do
    let map     = [] :: [(k, v)]
        map'    = "\"\"" -- encode map
    curtime <- round <$> liftIO getPOSIXTime
    res <- withPostgreSQLConn pool $ \ conn ->
        query conn qryLookupSession (key, curtime - storeSettingsSessionTimeout stos) :: IO [Only B.ByteString]
    case res of
        [Only _]    -> backend pool stos key map
        _           -> dbStore' pool stos Nothing

backend :: (WithPostgreSQLConn a, Serialize k, Eq k, Serialize v, MonadIO m) => a -> StoreSettings -> B.ByteString -> [(k, v)] -> IO (Session m k v, IO B.ByteString)
backend pool stos key mappe = do
    newKey <- storeSettingsKeyGen stos
    withPostgreSQLConn pool $ \conn ->
        execute conn qryUpdateKey (newKey, key)
    return ( (
        (reader pool newKey mappe)
      , (writer pool newKey mappe) )
     , return newKey )

reader :: (WithPostgreSQLConn a, Serialize k, Eq k, Serialize v, MonadIO m) => a -> B.ByteString -> [(k, v)] -> k -> m (Maybe v)
reader pool key mappe k = do
    curtime <- round <$> liftIO getPOSIXTime
    res <- liftIO $ withPostgreSQLConn pool $ \conn -> do
        void $ execute conn qryLookupSession' (curtime :: Int64, key)
        query conn qryLookupSession'' (Only key)
    case res of
        [Only store']    -> case decode (fromBinary store') of
            Right store     -> return $ k `lookup` store
            Left error      -> return Nothing
        []              -> return Nothing

writer :: (WithPostgreSQLConn a, Serialize k, Eq k, Serialize v, MonadIO m) => a -> B.ByteString -> [(k, v)] -> k -> v -> m ()
writer pool key mappe k v = do
    curtime <- round <$> liftIO getPOSIXTime
    [Only store] <- liftIO $ withPostgreSQLConn pool $ \conn ->
        query conn qryLookupSession'' (Only key)
    let store'      = case decode (fromBinary store) of
            Right s             -> s
            _                   -> []
        store''     = ((k,v):) . filter ((/=k) . fst) $ store'
        store'''    = encode store''
    liftIO $ withPostgreSQLConn pool $ \conn ->
        void $ execute conn qryUpdateSession (Binary store''', curtime :: Int64, key)


ignoreSqlError :: SqlError -> IO ()
ignoreSqlError _ = pure ()

unerror :: IO a -> IO ()
unerror action = void action `catch` ignoreSqlError
