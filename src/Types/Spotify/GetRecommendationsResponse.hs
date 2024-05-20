{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Types.Spotify.GetRecommendationsResponse
  ( GetRecommendationsResponse (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Types.Spotify.Track as Track

data GetRecommendationsResponse = GetRecommendationsResponse
  { tracks :: [Track.Track]
  }
  deriving (Generic, Show)

instance FromJSON GetRecommendationsResponse

instance ToJSON GetRecommendationsResponse
