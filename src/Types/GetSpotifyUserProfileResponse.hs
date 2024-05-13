{-# LANGUAGE DeriveGeneric #-}

module Types.GetSpotifyUserProfileResponse where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data GetSpotifyUserProfileResponse = GetSpotifyUserProfileResponse
  { displayName :: Text
  , profileImageUrl :: Maybe Text
  }
  deriving (Generic)

instance ToJSON GetSpotifyUserProfileResponse
