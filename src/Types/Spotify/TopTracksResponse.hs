{-# LANGUAGE DeriveGeneric #-}

module Types.Spotify.TopTracksResponse
  ( TopTracksResponse (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Types.Spotify.Track as Track

data TopTracksResponse = TopTracksResponse
  { next :: Maybe Text,
    offset :: Maybe Int,
    items :: Maybe [Track.Track]
  }
  deriving (Generic, Show)

instance ToJSON TopTracksResponse

instance FromJSON TopTracksResponse
