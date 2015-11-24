-- copied from https://github.com/singpolyma/wai-session/blob/master/example/Main.hs and modified
module Main where

import Data.Default (def)
import Data.String (fromString)
import Database.PostgreSQL.Simple
import Numeric (showHex)
import Network.Wai
import Network.Wai.Session (withSession, Session)
import Network.Wai.Session.PostgreSQL ( dbStore, StoreSettings (..) )
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (ok200)
import System.Entropy (getEntropy)

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vault.Lazy as Vault

app :: Vault.Key (Session IO String String) -> Application
app session env = (>>=) $ do
    u <- sessionLookup "u"
    sessionInsert "u" insertThis
    return $ responseLBS ok200 [] $ maybe (fromString "Nothing") fromString u
    where
        insertThis = show $ pathInfo env
        Just (sessionLookup, sessionInsert) = Vault.lookup session (vault env)

main :: IO ()
main = do
    conn <- dbconnect
    session <- Vault.newKey
    store <- dbStore conn settings
    run 3000 $ withSession store (fromString "SESSION") def session $ app session

settings :: StoreSettings
settings = StoreSettings
    { storeSettingsSessionTimeout=3600
    , storeSettingsKeyGen=genSessionKey
    }

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
