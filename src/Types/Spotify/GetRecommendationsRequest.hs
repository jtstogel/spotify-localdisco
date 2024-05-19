{-# LANGUAGE DeriveGeneric #-}

module Types.Spotify.GetRecommendationsRequest
  ( GetRecommendationsRequest(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data GetRecommendationsRequest = GetRecommendationsRequest
  { seedArtists :: ![Text]
  , seedTracks :: ![Text]
  , limit :: !Int
  }
  deriving (Generic, Show)

instance FromJSON GetRecommendationsRequest
instance ToJSON GetRecommendationsRequest
