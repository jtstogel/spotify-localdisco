{-# LANGUAGE DeriveGeneric #-}

module Types.Spotify.ListSavedTracksRequest
  ( ListSavedTracksRequest (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data ListSavedTracksRequest = ListSavedTracksRequest
  { offset :: !(Maybe Int),
    limit :: !(Maybe Int)
  }
  deriving (Generic, Show)

instance FromJSON ListSavedTracksRequest

instance ToJSON ListSavedTracksRequest
