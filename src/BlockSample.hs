{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module BlockSample where

import           Control.Monad (mzero)
import           Data.Aeson
import           Data.Int
import           Data.Word
import           Data.Text (Text)

data BlockSample = BlockSample {
    bsMagic            :: Word32
  , bsBpVersion        :: Text
  , bsBlockNo          :: Word64
  , bsSlotNo           :: Word64
  , bsBlockHash        :: Text
  , bsSize             :: Word32
  , bsHeaderRemoteAddr :: Text
  , bsHeaderRemotePort :: Word16
  , bsHeaderDelta      :: Int32
  , bsBlockReqDelta    :: Int32
  , bsBlockRspDelta    :: Int32
  , bsBlockAdoptDelta  :: Int32
  , bsBlockRemoteAddr  :: Text
  , bsBlockRemotePort  :: Word16
  , bsLocalAddr        :: Text
  , bsLocalPort        :: Word16
} deriving Show

instance FromJSON BlockSample where
  parseJSON (Object o) = BlockSample
                    <$> (read <$> o .: "magic")
                    <*> o .: "bpVersion"
                    <*> (read <$> o .: "blockNo")
                    <*> (read <$> o .: "slotNo")
                    <*> o .: "blockHash"
                    <*> (read <$> o .: "blockSize")
                    <*> o .: "headerRemoteAddr"
                    <*> (read <$> o .: "headerRemotePort")
                    <*> (read <$> o .: "headerDelta")
                    <*> (read <$> o .: "blockReqDelta")
                    <*> (read <$> o .: "blockRspDelta")
                    <*> (read <$> o .: "blockAdoptDelta")
                    <*> o .: "blockRemoteAddress"
                    <*> (read <$> o .: "blockRemotePort")
                    <*> o .: "blockLocalAddress"
                    <*> (read <$> o .: "blockLocalPort")
  parseJSON _          = mzero
