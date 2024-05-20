{-# LANGUAGE DeriveGeneric #-}

module Types.Spotify.ListFollowedArtistsRequest
  ( ListFollowedArtistsRequest (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data ListFollowedArtistsRequest = ListFollowedArtistsRequest
  { after :: !(Maybe Text),
    limit :: !(Maybe Int)
  }
  deriving (Generic, Show)

instance FromJSON ListFollowedArtistsRequest

instance ToJSON ListFollowedArtistsRequest
