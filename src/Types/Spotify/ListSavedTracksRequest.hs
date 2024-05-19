{-# LANGUAGE DeriveGeneric #-}

module Types.Spotify.ListSavedTracksRequest
  ( ListSavedTracksRequest(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import qualified Types.Spotify.Track as Track

data ListSavedTracksRequest = ListSavedTracksRequest
  { offset :: !(Maybe Int)
  , limit :: !(Maybe Int)
  }
  deriving (Generic, Show)

instance FromJSON ListSavedTracksRequest
instance ToJSON ListSavedTracksRequest
