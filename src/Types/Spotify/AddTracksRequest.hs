{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{- HLINT ignore "Use newtype instead of data" -}

module Types.Spotify.AddTracksRequest
  ( AddTracksRequest (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data AddTracksRequest = AddTracksRequest
  { uris :: ![Text]
  }
  deriving (Generic, Show)

instance FromJSON AddTracksRequest

instance ToJSON AddTracksRequest
