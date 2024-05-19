{-# LANGUAGE DeriveGeneric #-}

module Types.Spotify.Artist
  ( Artist(..)
  )
  where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data Artist = Artist
  { name :: Text
  , href :: Text
  , id :: Text
  }
  deriving (Generic, Show)

instance FromJSON Artist
instance ToJSON Artist
