{-# LANGUAGE DeriveGeneric #-}

module Types.Spotify.Track
  ( Track (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Types.Spotify.Artist as Artist

data Track = Track
  { id :: Text,
    artists :: Maybe [Artist.Artist],
    name :: Text,
    uri :: Text
  }
  deriving (Generic, Show)

instance FromJSON Track

instance ToJSON Track
