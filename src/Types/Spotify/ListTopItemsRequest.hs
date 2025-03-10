{-# LANGUAGE DeriveGeneric #-}

module Types.Spotify.ListTopItemsRequest
  ( ListTopItemsRequest(..)
  )
  where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data ListTopItemsRequest = ListTopItemsRequest
  { timeRange :: !(Maybe Text)
  , limit :: !(Maybe Int)
  , offset :: !(Maybe Int)
  }
  deriving (Generic, Show)

instance ToJSON ListTopItemsRequest

instance FromJSON ListTopItemsRequest
