{-# LANGUAGE DeriveGeneric #-}

module Types.Spotify.TopItems where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

newtype Item = Item
  { id :: Text
  }
  deriving (Generic, Show)

instance FromJSON Item

data TopItemsResponse = TopItemsResponse
  { next :: Maybe Text,
    offset :: Maybe Int,
    items :: [Item]
  }
  deriving (Generic, Show)

instance FromJSON TopItemsResponse
