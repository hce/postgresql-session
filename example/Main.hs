{-# LANGUAGE OverloadedStrings #-}
-- copied from https://github.com/singpolyma/wai-session/blob/master/example/Main.hs and modified
module Main where

import Control.Monad
import Data.Default (def)
import Data.String (fromString)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple
import Numeric (showHex)
import Network.Wai
import Network.Wai.Session (withSession, Session)
import Network.Wai.Session.PostgreSQL
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (ok200)
import System.Entropy (getEntropy)

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vault.Lazy as Vault

app :: Connection -> Vault.Key (Session IO String String) -> Application
app conn session env = (>>=) $ do
    u <- (,) <$> sessionLookup "u" <*> sessionLookup "d"
    when (pathInfo env == ["login"]) $ do
        putStrLn "Login -- new session required!"
        conn' <- fromSimpleConnection conn
        clearSession conn' "SESSION" env
    sessionInsert "u" insertThis
    curtime <- round <$> getPOSIXTime
    sessionInsert "d" $ show curtime
    return $ responseLBS ok200 [] $ maybe (fromString "Nothing") fromString $ Just $ show u
    where
        insertThis = show $ pathInfo env
        Just (sessionLookup, sessionInsert) = Vault.lookup session (vault env)

main :: IO ()
main = do
    conn <- dbconnect
    session <- Vault.newKey
    conn' <- fromSimpleConnection conn
    store <- dbStore conn' def
    purger conn' def
    run 3000 $ withSession store (fromString "SESSION") def session $ app conn session

genSessionKey :: IO B.ByteString
genSessionKey =
    TE.encodeUtf8 . prettyPrint <$> getEntropy 24

dbconnect :: IO Connection
dbconnect = do
    let connectInfo = ConnectInfo {
          connectHost = "localhost"
        , connectPort = 5432
        , connectUser = "demo"
        , connectPassword = "omed"
        , connectDatabase = "demodb" }
    connectPostgreSQL $ postgreSQLConnectionString connectInfo

prettyPrint :: B.ByteString -> T.Text
prettyPrint = T.pack . concat . map (flip showHex "") . B.unpack
