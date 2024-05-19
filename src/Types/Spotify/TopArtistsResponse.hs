{-# LANGUAGE DeriveGeneric #-}

module Types.Spotify.TopArtistsResponse
  ( TopArtistsResponse(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import qualified Types.Spotify.Artist as Artist

data TopArtistsResponse = TopArtistsResponse
  { offset :: Maybe Int,
    items :: Maybe [Artist.Artist]
  }
  deriving (Generic, Show)

instance FromJSON TopArtistsResponse
instance ToJSON TopArtistsResponse
