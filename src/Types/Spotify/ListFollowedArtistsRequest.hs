{-# LANGUAGE DeriveGeneric #-}

module Types.Spotify.ListFollowedArtistsRequest
  ( ListFollowedArtistsRequest(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import qualified Types.Spotify.Artist as Artist

data ListFollowedArtistsRequest = ListFollowedArtistsRequest
  { after :: !(Maybe Text)
  , limit :: !(Maybe Int)
  }
  deriving (Generic, Show)

instance FromJSON ListFollowedArtistsRequest
instance ToJSON ListFollowedArtistsRequest
