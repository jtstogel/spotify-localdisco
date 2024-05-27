{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Types.Spotify.GetArtistTopTracksRequest
  ( GetArtistTopTracksRequest (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data GetArtistTopTracksRequest = GetArtistTopTracksRequest
  { artistId :: !Text
  }
  deriving (Generic, Show)

instance FromJSON GetArtistTopTracksRequest

instance ToJSON GetArtistTopTracksRequest
