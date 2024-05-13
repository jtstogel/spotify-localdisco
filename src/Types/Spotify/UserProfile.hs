{-# LANGUAGE DeriveGeneric #-}

module Types.Spotify.UserProfile where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

newtype UserProfileImage = UserProfileImage
  { url :: Text
  }
  deriving (Generic, Show)

instance FromJSON UserProfileImage

data UserProfileResponse = UserProfileResponse
  { display_name :: Text
  , images :: [UserProfileImage]
  }
  deriving (Generic, Show)

instance FromJSON UserProfileResponse
