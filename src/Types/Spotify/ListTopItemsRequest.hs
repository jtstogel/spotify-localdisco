{-# LANGUAGE DeriveGeneric #-}

module Types.Spotify.ListTopItemsRequest
  ( ListTopItemsRequest(..)
  )
  where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import qualified Types.Spotify.Artist as Artist

data ListTopItemsRequest = ListTopItemsRequest
  { timeRange :: !(Maybe Text)
  , limit :: !(Maybe Int)
  , offset :: !(Maybe Int)
  }
  deriving (Generic, Show)

instance FromJSON ListTopItemsRequest
