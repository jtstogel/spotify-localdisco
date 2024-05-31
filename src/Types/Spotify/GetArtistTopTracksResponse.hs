{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- HLINT ignore "Use newtype instead of data" -}

module Types.Spotify.GetArtistTopTracksResponse
  ( GetArtistTopTracksResponse (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Types.Spotify.Track as Track

data GetArtistTopTracksResponse = GetArtistTopTracksResponse
  { tracks :: ![Track.Track]
  }
  deriving (Generic, Show)

instance FromJSON GetArtistTopTracksResponse

instance ToJSON GetArtistTopTracksResponse
