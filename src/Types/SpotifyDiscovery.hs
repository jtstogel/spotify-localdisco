{-# LANGUAGE DeriveGeneric #-}

module Types.SpotifyDiscovery
  ( SpotifyDiscovery(..)
  , SpotifyDiscoveryStatus(..)
  )
  where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import qualified Types.Spotify.Artist as Artist
import qualified Types.Spotify.Track as Track

data SpotifyDiscovery = SpotifyDiscovery
  { artists :: ![Text]
  }
  deriving (Generic, Show)

data SpotifyDiscoveryStatus = SpotifyDiscoveryStatus
  { message :: Maybe Text
  , discovery :: Maybe SpotifyDiscovery
  }
  deriving (Generic, Show)

instance FromJSON SpotifyDiscovery
instance ToJSON SpotifyDiscovery
instance FromJSON SpotifyDiscoveryStatus
instance ToJSON SpotifyDiscoveryStatus
