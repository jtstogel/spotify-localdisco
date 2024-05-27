{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Types.Spotify.UserProfile
  ( UserProfile (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Types.Spotify.Track as Track

data UserProfile = UserProfile
  { user_id :: Text
  }
  deriving (Generic, Show)

instance FromJSON UserProfile

instance ToJSON UserProfile
