{-# LANGUAGE DeriveGeneric #-}

module Types.Spotify.GetRecommendationsRequest
  ( GetRecommendationsRequest (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data GetRecommendationsRequest = GetRecommendationsRequest
  { seedArtists :: ![Text],
    seedTracks :: ![Text],
    limit :: !Int
  }
  deriving (Generic, Show)

instance FromJSON GetRecommendationsRequest

instance ToJSON GetRecommendationsRequest
