{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.Logger (runNoLoggingT)
import           Control.Monad.Reader
import qualified Data.Aeson
import qualified Data.ByteString.UTF8 as BU
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time.Clock
import           Database.Persist.Postgresql
import           Network.MQTT.Client
import           Network.MQTT.Topic
import           Network.URI (parseURI)
import           System.Environment (lookupEnv)
import           Text.Printf

import qualified Schema as S
import BlockSample

main :: IO ()
main = do
  dbCon <- initDb
  runSqlPool (runMigration S.migrateAll) dbCon
  mq <- atomically $ newTBQueue 100

  mqttHost <- fromMaybe "127.0.0.1" <$> lookupEnv "MQTT_HOST"
  mqttPort <- fromMaybe "1883" <$> lookupEnv "MQTT_PORT"
  mqttUser <- lookupEnv "MQTT_USER"
  mqttPw   <- lookupEnv "MQTT_PW"

  let uri_m = case (mqttUser, mqttPw) of
                   (Nothing, Nothing) -> parseURI $ "mqtt://" ++ mqttHost ++ ":" ++ mqttPort
                   (Just u, Just p)   -> parseURI $ "mqtt://" ++ u ++ ":" ++ p ++ "@" ++ mqttHost ++ ":" ++ mqttPort
                   _                  -> error "either set both MQTT_USER and MQTT_PW or leave both blank"
  when (isNothing uri_m) $
    error "failed to parse mqtt uri\n"
  mc <- connectURI mqttConfig{ _msgCB = SimpleCallback (msgReceived mq) } $ fromJust uri_m
  withAsync (insertMsg mq dbCon) $ \dbaid -> do
    print =<< subscribe mc [("blockperf/#", subOptions { _subQoS = QoS1 })] []
    waitForClient mc
    cancel dbaid

 where
  initDb :: IO ConnectionPool
  initDb = do
    dbConstr <- fromMaybe "host=127.0.0.1 port=5432 dbname=slurpdb" <$> lookupEnv "SLURP_DB"
    runNoLoggingT $ createPostgresqlPool (BU.fromString dbConstr) 1

  insertMsg mq con =
    forever $ do
      (t, bs) <- atomically $ readTBQueue mq
      let (Just provider) = T.takeWhile (\c -> c /= '/') <$> (T.stripPrefix "blockperf/" $ unTopic t)
      printf "%s %s Inserting block %d, delay %d\n" (unTopic t) (bsLocalAddr bs)
          (bsBlockNo bs)
          (bsHeaderDelta bs + bsBlockReqDelta bs + bsBlockRspDelta bs +
           bsBlockAdoptDelta bs)

      void $ runSqlPool (insertBlockSample provider bs) con

  msgReceived mq _ t m _ = do
    case Data.Aeson.eitherDecode m :: Either String BlockSample of
         Left err -> do
           -- XXX excessive number of invalid messages should cause the user to be blacklisted.
           now <- getCurrentTime
           printf "%s: %s from %s\n" (show now) err (show m)
         Right bs -> do
           if sane bs
             then atomically $ writeTBQueue mq (t, bs)
             else printf "Ignoring %s\n" (show bs)

sane :: BlockSample -> Bool
sane BlockSample{..} =
  not
    (T.length bsBpVersion > 10 ||
     T.length bsBlockHash > 128 ||
     T.length bsHeaderRemoteAddr > 32 ||
     T.length bsBlockRemoteAddr > 32 ||
     T.length bsLocalAddr > 32 ||
     bsSize == 0 ||
     bsSize > 10_000_000 ||
     bsHeaderDelta > 60000 ||
     bsBlockReqDelta > 10000 ||
     bsBlockRspDelta > 60000 ||
     bsBlockAdoptDelta > 10000)


insertBlockSample :: T.Text
                  -> BlockSample
                  -> ReaderT SqlBackend IO (Either SomeException (Key S.BlockSample))
insertBlockSample provider BlockSample{..} = do
  newProvider <- upsert (S.Provider provider) []
  let relay = S.Relay bsLocalAddr bsLocalPort bsMagic (entityKey newProvider)
      hPeer = S.Peer bsHeaderRemoteAddr bsHeaderRemotePort
      bPeer = S.Peer bsBlockRemoteAddr bsBlockRemotePort
  newRelay <- upsert relay []
  newHPeer <- upsert hPeer []
  newBPeer <- upsert bPeer []
  let bs' = S.BlockSample (entityKey newRelay) bsBlockNo bsSlotNo bsSize bsBlockHash
              (entityKey newHPeer) (entityKey newBPeer) bsHeaderDelta bsBlockReqDelta
              bsBlockRspDelta bsBlockAdoptDelta
  newBs <- upsert bs' []
  return $ Right $ entityKey newBs

