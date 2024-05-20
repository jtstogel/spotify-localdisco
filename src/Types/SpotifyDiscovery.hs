{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Types.SpotifyDiscovery
  ( SpotifyDiscovery (..),
    SpotifyDiscoveryStatus (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data SpotifyDiscovery = SpotifyDiscovery
  { artists :: ![Text]
  }
  deriving (Generic, Show)

data SpotifyDiscoveryStatus = SpotifyDiscoveryStatus
  { message :: Maybe Text,
    discovery :: Maybe SpotifyDiscovery
  }
  deriving (Generic, Show)

instance FromJSON SpotifyDiscovery

instance ToJSON SpotifyDiscovery

instance FromJSON SpotifyDiscoveryStatus

instance ToJSON SpotifyDiscoveryStatus
