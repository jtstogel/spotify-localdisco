{-# LANGUAGE DeriveGeneric #-}

module Types.Spotify.TopTracksResponse
  ( TopTracksResponse(..)
  )
  where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import qualified Types.Spotify.Track as Track

data TopTracksResponse = TopTracksResponse
  { next :: Maybe Text,
    offset :: Maybe Int,
    items :: Maybe [Track.Track]
  }
  deriving (Generic, Show)

instance FromJSON TopTracksResponse
