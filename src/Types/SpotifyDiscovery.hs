{-# LANGUAGE DeriveGeneric #-}

module Types.SpotifyDiscovery
  ( SpotifyDiscovery(..)
  )
  where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import qualified Types.Spotify.Artist as Artist
import qualified Types.Spotify.Track as Track

data SpotifyDiscovery = SpotifyDiscovery
  { spotifyArtists :: ![Text]
  , ticketmasterArtists :: ![Text]
  }
  deriving (Generic, Show)

instance FromJSON SpotifyDiscovery
instance ToJSON SpotifyDiscovery
