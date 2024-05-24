{-# LANGUAGE DeriveGeneric #-}

module Types.CreatePlaylistJobRequest
  ( CreatePlaylistJobRequest(..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data CreatePlaylistJobRequest = CreatePlaylistJobRequest
  { spotifyAccessToken :: !Text,
    postalCode :: !Text,
    radiusMiles :: !Int,
    days :: !Int,
    spideringDepth :: !Int
  }
  deriving (Generic)

instance ToJSON CreatePlaylistJobRequest

instance FromJSON CreatePlaylistJobRequest
