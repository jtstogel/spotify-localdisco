{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- HLINT ignore "Use newtype instead of data" -}

module Types.Spotify.AddTracksResponse
  ( AddTracksResponse (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data AddTracksResponse = AddTracksResponse
  { snapshot_id :: !Text
  }
  deriving (Generic, Show)

instance FromJSON AddTracksResponse

instance ToJSON AddTracksResponse
