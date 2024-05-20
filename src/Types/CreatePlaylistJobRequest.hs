{-# LANGUAGE DeriveGeneric #-}

module Types.CreatePlaylistJobRequest where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data CreatePlaylistJobRequest = CreatePlaylistJobRequest
  { spotifyAccessToken :: !Text
  , postalCode :: !Text
  , radiusMiles :: !Int
  , days :: !Int
  }
  deriving (Generic)

instance ToJSON CreatePlaylistJobRequest
instance FromJSON CreatePlaylistJobRequest
