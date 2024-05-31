{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- HLINT ignore "Use newtype instead of data" -}

module Types.Spotify.UserProfile
  ( UserProfile (..),
    Image (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Image = Image
  { url :: Text
  }
  deriving (Generic, Show)

data UserProfile = UserProfile
  { id :: Text,
    display_name :: Text,
    images :: Maybe [Image]
  }
  deriving (Generic, Show)

instance FromJSON Image

instance ToJSON Image

instance FromJSON UserProfile

instance ToJSON UserProfile
