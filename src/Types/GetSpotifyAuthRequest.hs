{-# LANGUAGE DeriveGeneric #-}

module Types.GetSpotifyAuthRequest where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data GetSpotifyAuthRequest = GetSpotifyAuthRequest
  { code :: Text
  , redirectUri :: Text
  }
  deriving (Generic)

instance FromJSON GetSpotifyAuthRequest
