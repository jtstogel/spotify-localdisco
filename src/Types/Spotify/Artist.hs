{-# LANGUAGE DeriveGeneric #-}

module Types.Spotify.Artist
  ( Artist (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Artist = Artist
  { name :: Text,
    href :: Text,
    id :: Text
  }
  deriving (Generic, Show)

instance FromJSON Artist

instance ToJSON Artist
