{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Types.Spotify.CreatePlaylistRequest
  ( CreatePlaylistRequest (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data CreatePlaylistRequest = CreatePlaylistRequest
  { name :: !Text,
    description :: !Text,
    public :: !Bool
  }
  deriving (Generic, Show)

instance FromJSON CreatePlaylistRequest

instance ToJSON CreatePlaylistRequest
