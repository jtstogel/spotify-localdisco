{-# LANGUAGE DeriveGeneric #-}

module Types.Spotify.TopArtistsResponse
  ( TopArtistsResponse (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Types.Spotify.Artist as Artist

data TopArtistsResponse = TopArtistsResponse
  { offset :: Maybe Int,
    items :: Maybe [Artist.Artist]
  }
  deriving (Generic, Show)

instance FromJSON TopArtistsResponse

instance ToJSON TopArtistsResponse
