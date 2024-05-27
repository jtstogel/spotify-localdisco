{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Types.SpotifyProfile
  ( SpotifyProfile (..)
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data SpotifyProfile = SpotifyProfile
  { displayName :: !Text,
    profileImageUrl :: !Text
  }
  deriving (Generic, Show)

instance FromJSON SpotifyProfile

instance ToJSON SpotifyProfile
