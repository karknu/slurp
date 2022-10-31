{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TypeOperators     #-}

module Servant.IpRegistry.Api where

import           Data.Aeson
import           Data.Text (Text)
import           Servant

data GeoIP = GeoIP {
    giCompany   :: Maybe Text
  , giAsn       :: Maybe Int
  , giContinent :: Maybe Text
  , giCountry   :: Maybe Text
  , giRegion    :: Maybe Text
  , giCity      :: Maybe Text
  , giLatitude  :: Maybe Double
  , giLongitude :: Maybe Double
} deriving Show

instance FromJSON GeoIP where
  parseJSON = withObject "GeoIP" $ \v -> do
    company     <- v .: "company" >>= (.: "name")
    asn         <- v .: "connection" >>= (.: "asn")
    locationObj <- v .: "location"
    continent   <- locationObj .: "continent" >>= (.: "name")
    country     <- locationObj .: "country" >>= (.: "name")
    region      <- locationObj .: "region" >>= (.: "name")
    city        <- locationObj .: "city"
    latitude    <- locationObj .: "latitude"
    longitude   <- locationObj .: "longitude"

    return $ GeoIP company asn continent country region city latitude longitude


type GeoIPApi = QueryParam "key" Text :> Get '[JSON] GeoIP

