{-# LANGUAGE DeriveGeneric #-}

module Types.CreatePlaylistJobRequest
  ( CreatePlaylistJobRequest(..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data CreatePlaylistJobRequest = CreatePlaylistJobRequest
  { postalCode :: !Text,
    radiusMiles :: !Int,
    startTime :: !String,  -- in ISO8601 format
    endTime :: !String,  -- in ISO8601 format
    spideringDepth :: !Int
  }
  deriving (Generic)

instance ToJSON CreatePlaylistJobRequest

instance FromJSON CreatePlaylistJobRequest
