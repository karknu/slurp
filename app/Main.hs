{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Exception (SomeException (..), catch)
import           Control.Monad.Logger (runNoLoggingT)
import           Control.Monad.Reader
import qualified Data.Aeson
import qualified Data.ByteString.UTF8 as BU
import           Data.Maybe
import qualified Data.Text as T
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
  mc <- connectURI mqttConfig{ _msgCB = SimpleCallback (msgReceived dbCon) } $ fromJust uri_m
  print =<< subscribe mc [("blockperf/+", subOptions)] []
  waitForClient mc
 where
  initDb :: IO ConnectionPool
  initDb = do
    dbConstr <- fromMaybe "host=127.0.0.1 port=5432 dbname=slurpdb" <$> lookupEnv "SLURP_DB"
    runNoLoggingT $ createPostgresqlPool (BU.fromString dbConstr) 1


  msgReceived con _ t m _ = do
    case Data.Aeson.eitherDecode m :: Either String BlockSample of
         Left err -> print err
         Right bs -> do
           if sane bs
             then do
               let (Just provider) = T.stripPrefix "blockperf/" $ unTopic t
               printf "%s %s Inserting block %d, delay %d\n" (unTopic t) (bsLocalAddr bs)
                 (bsBlockNo bs)
                 (bsHeaderDelta bs + bsBlockReqDelta bs + bsBlockRspDelta bs +
                  bsBlockAdoptDelta bs)
               void $ runSqlPool (insertBlockSample provider bs) con
             else printf "Ignoring %s\n" (show bs)

sane :: BlockSample -> Bool
sane BlockSample{..} =
  not
    (T.length bsBpVersion > 10 ||
     T.length bsBlockHash > 128 ||
     T.length bsHeaderRemoteAddr > 32 ||
     T.length bsBlockRemoteAddr > 32 ||
     T.length bsLocalAddr > 32 ||
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

