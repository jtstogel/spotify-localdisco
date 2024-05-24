{-# LANGUAGE DeriveGeneric #-}

module Types.Spotify.ListSavedTracksResponse
  ( ListSavedTracksResponse (..),
    SavedTrackObject (..),
  )
where

import Data.Aeson
import GHC.Generics (Generic)
import qualified Types.Spotify.Track as Track

newtype SavedTrackObject = SavedTrackObject
  { track :: Track.Track
  }
  deriving (Generic, Show)

data ListSavedTracksResponse = ListSavedTracksResponse
  { offset :: Maybe Int,
    items :: Maybe [SavedTrackObject]
  }
  deriving (Generic, Show)

instance FromJSON ListSavedTracksResponse

instance FromJSON SavedTrackObject

instance ToJSON ListSavedTracksResponse

instance ToJSON SavedTrackObject
