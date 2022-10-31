{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}


module Servant.IpRegistry.Client where

import           Data.IP
import           Data.Proxy
import           Data.Text (Text)
import           Servant.Client

import           Servant.IpRegistry.Api

runGeoIP :: ClientEnv
         -> Text
         -> IP
         -> IO (Either ClientError GeoIP)
runGeoIP env@ClientEnv{..} key addr = do
  let env' = env {
               baseUrl = baseUrl {baseUrlPath = baseUrlPath baseUrl ++ "/" ++ show addr }
             }
  res <- runClientM doPost env'
  case res of
       Right rsp -> return $ Right rsp
       Left  err -> return $ Left err
 where
  doPost :: ClientM GeoIP
  doPost = client (Proxy @GeoIPApi) (Just key)
