{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Schema where

import           Data.Text (Text)
import           Data.Int
import           Data.Word
import           Database.Persist.TH

share
    [ mkPersist sqlSettings
    , mkMigrate "migrateAll"
    ] [persistLowerCase|
  Provider
    name           Text
    UniqueProvider name
    deriving Show

  Relay
    addr        Text
    port        Word16
    magic       Word32
    provider    ProviderId
    UniqueRelay addr port magic provider
    deriving Show

  Peer
    addr Text
    port Word16
    UniquePeer addr port
    deriving Show

  BlockSample
    relay             RelayId
    blockNo           Word64
    slotNo            Word64
    size              Word32
    hash              Text
    headerFrom        PeerId
    blockFrom         PeerId
    headerDelta       Int32
    blockReqDelta     Int32
    blockRspDelta     Int32
    blockAdoptDelta   Int32
    UniqueBlockSample relay hash
    deriving Show
|]

