{-# LANGUAGE DeriveGeneric #-}

module Types.Spotify.GetRecommendationsResponse
  ( GetRecommendationsResponse(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import qualified Types.Spotify.Track as Track

data GetRecommendationsResponse = GetRecommendationsResponse
  { tracks :: [Track.Track]
  }
  deriving (Generic, Show)

instance FromJSON GetRecommendationsResponse
instance ToJSON GetRecommendationsResponse
