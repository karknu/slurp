{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.Logger (runNoLoggingT)
import           Control.Monad.Reader
import qualified Data.Aeson
import qualified Data.ByteString.UTF8 as BU
import           Data.IP
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time.Clock
import           Database.Persist.Postgresql
import           Network.HTTP.Client.TLS (newTlsManager)
import           Network.MQTT.Client
import           Network.MQTT.Topic
import           Network.URI (parseURI)
import           Servant.Client (mkClientEnv, parseBaseUrl)
import           System.Environment (lookupEnv)
import           Text.Printf
import           Text.Read

import           BlockSample
import qualified Schema as S
import qualified Servant.IpRegistry.Api as Ir
import           Servant.IpRegistry.Client

main :: IO ()
main = do
  dbCon <- initDb
  runSqlPool (runMigration S.migrateAll) dbCon
  mq <- atomically $ newTBQueue 100

  mqttHost <- fromMaybe "127.0.0.1" <$> lookupEnv "MQTT_HOST"
  mqttPort <- fromMaybe "1883" <$> lookupEnv "MQTT_PORT"
  mqttUser <- lookupEnv "MQTT_USER"
  mqttPw   <- lookupEnv "MQTT_PW"
  iprKey   <- lookupEnv "IPR_KEY"

  let uri_m = case (mqttUser, mqttPw) of
                   (Nothing, Nothing) -> parseURI $ "mqtt://" ++ mqttHost ++ ":" ++ mqttPort
                   (Just u, Just p)   -> parseURI $ "mqtt://" ++ u ++ ":" ++ p ++ "@" ++ mqttHost ++ ":" ++ mqttPort
                   _                  -> error "either set both MQTT_USER and MQTT_PW or leave both blank"
  when (isNothing uri_m) $
    error "failed to parse mqtt uri\n"
  mc <- connectURI mqttConfig{ _msgCB = SimpleCallback (msgReceived mq) } $ fromJust uri_m
  withAsync (insertMsg mq dbCon) $ \dbaid ->
    withAsync (geoLocation dbCon (T.pack <$> iprKey)) $ \geoid -> do
      print =<< subscribe mc [("blockperf/#", subOptions { _subQoS = QoS1 })] []
      waitForClient mc
      cancel dbaid
      cancel geoid

 where
  initDb :: IO ConnectionPool
  initDb = do
    dbConstr <- fromMaybe "host=127.0.0.1 port=5432 dbname=slurpdb" <$> lookupEnv "SLURP_DB"
    runNoLoggingT $ createPostgresqlPool (BU.fromString dbConstr) 2

  -- Monitor the database for peers with missing geolocation data.
  -- Missing data is fetched from www.ipregistry.co, as long as an API key is provided.
  -- TODO: Monitor both relay and peer table,
  geoLocation :: ConnectionPool -> Maybe T.Text -> IO ()
  geoLocation _ Nothing = return () -- No key, geoip lookup disabled
  geoLocation con (Just key) = do
    env <- mkClientEnv <$> newTlsManager
                       <*> parseBaseUrl "https://api.ipregistry.co"
    forever $ do
      peer_m <- runSqlPool (selectFirst [S.PeerGeoip ==. Nothing] []) con
      case peer_m of
           Nothing  -> threadDelay 10_000_000
           Just ent -> do
             let addr = S.peerAddr $ entityVal ent
             geoKey_m <- getGeoIP con addr
             case geoKey_m of
                  Just _ -> do
                    runSqlPool (updateWhere [S.PeerId ==. entityKey ent]
                                 [S.PeerGeoip =. geoKey_m]
                               ) con
                  Nothing -> do
                    res <- runGeoIP env key (read $ T.unpack addr)
                    case res of
                         Left err -> do
                           printf "geo ip failed, %s\n" $ show err
                           -- TODO: Differentiate between looking up rfc1918 IPs and other errors.
                           --       Both return 401.
                           threadDelay 60_000_000
                         Right Ir.GeoIP{..} -> do
                           let geoip = S.GeoIP addr giCompany giAsn giContinent giCountry giRegion
                                         giCity giLatitude giLongitude
                           void $ runSqlPool (upsert geoip []) con
                           printf "inserted geo data for %s\n" $ show addr
                           threadDelay 3_000_000


  insertMsg mq con =
    forever $ do
      (t, bs) <- atomically $ readTBQueue mq
      let (Just provider) = T.takeWhile (\c -> c /= '/') <$> (T.stripPrefix "blockperf/" $ unTopic t)
      printf "%s %s Inserting block %d, delay %d\n" (unTopic t) (bsLocalAddr bs)
          (bsBlockNo bs)
          (bsHeaderDelta bs + bsBlockReqDelta bs + bsBlockRspDelta bs +
           bsBlockAdoptDelta bs)

      geoIpH <- getGeoIP con (bsHeaderRemoteAddr bs)
      geoIpB <- getGeoIP con (bsBlockRemoteAddr bs)

      void $ runSqlPool (insertBlockSample provider geoIpH geoIpB bs) con

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
     bsBlockAdoptDelta > 10000 ||
     invalidAddress bsHeaderRemoteAddr ||
     invalidAddress bsBlockRemoteAddr ||
     invalidAddress bsLocalAddr)

 where
   invalidAddress addr =
     case readMaybe (T.unpack addr) :: Maybe IP of
           Nothing -> True
           Just _  -> False

getGeoIP :: ConnectionPool -> T.Text -> IO (Maybe (Key S.GeoIP))
getGeoIP con addr = do
  e_m <- runSqlPool (getBy $ S.UniqueGeoIP addr) con
  case e_m of
       Just e -> return $ Just $ entityKey e
       Nothing -> return Nothing

insertBlockSample :: T.Text
                  -> Maybe (Key S.GeoIP)
                  -> Maybe (Key S.GeoIP)
                  -> BlockSample
                  -> ReaderT SqlBackend IO (Either SomeException (Key S.BlockSample))
insertBlockSample provider geoH geoB BlockSample{..} = do
  newProvider <- upsert (S.Provider provider) []
  let relay = S.Relay bsLocalAddr bsLocalPort bsMagic (entityKey newProvider)
      hPeer = S.Peer bsHeaderRemoteAddr bsHeaderRemotePort geoH
      bPeer = S.Peer bsBlockRemoteAddr bsBlockRemotePort geoB
  newRelay <- upsert relay []
  newHPeer <- upsert hPeer []
  newBPeer <- upsert bPeer []
  let bs' = S.BlockSample (entityKey newRelay) bsBlockNo bsSlotNo bsSize bsBlockHash
              (entityKey newHPeer) (entityKey newBPeer) bsHeaderDelta bsBlockReqDelta
              bsBlockRspDelta bsBlockAdoptDelta
  newBs <- upsert bs' []
  return $ Right $ entityKey newBs

