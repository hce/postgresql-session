{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception.Base (assert)
import Control.Monad
import Data.Default (def)
import Data.String (fromString)
import Database.PostgreSQL.Simple
import Network.Wai.Session (withSession, Session)
import Network.Wai.Session.PostgreSQL

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

main :: IO ()
main = do
    conn <- dbconnect
    conn' <- fromSimpleConnection conn
    store <- dbStore conn' def
    purger conn' def

    ((lookupSess1, insertSess1), mknewsessid) <- store Nothing
    sessid <- mknewsessid

    insertSess1 ("foo" :: B.ByteString) ("bar" :: B.ByteString)

    l1 <- lookupSess1 "foo"
    assert (l1 == (Just "bar")) it

    l2 <- lookupSess1 "bar"
    assert (l2 == Nothing) it

    ((lookupSess2, insertSess2), mknewsessid) <- store $ Just sessid
    newsessid <- mknewsessid

    l3 <- lookupSess2 "foo"
    assert (l3 == (Just "bar")) it

    assert (newsessid == sessid) it

    let invalidsessid = "foobar"
    ((lookupSess3, insertSess3), mknewsessid) <- store $ Just invalidsessid
    newsessid2 <- mknewsessid

    assert (newsessid2 /= newsessid) it
    assert (newsessid2 /= invalidsessid) it

    l3 <- lookupSess3 "foo"
    assert (l3 == Nothing) it


it :: IO ()
it = return ()

dbconnect :: IO Connection
dbconnect = do
    let connectInfo = ConnectInfo {
          connectHost = "localhost"
        , connectPort = 5432
        , connectUser = "demo"
        , connectPassword = "omed"
        , connectDatabase = "demodb" }
    connectPostgreSQL $ postgreSQLConnectionString connectInfo
