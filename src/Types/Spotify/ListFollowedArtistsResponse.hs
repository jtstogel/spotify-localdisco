{-# LANGUAGE DeriveGeneric #-}

module Types.Spotify.ListFollowedArtistsResponse
  ( ListFollowedArtistsResponse(..)
  , ArtistsObject(..)
  , Cursor(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import qualified Types.Spotify.Artist as Artist

data Cursor = Cursor
  { after :: Maybe Text
  }
  deriving (Generic, Show)

data ArtistsObject = ArtistsObject
  { items :: Maybe [Artist.Artist]
  , cursor :: Maybe Cursor
  }
  deriving (Generic, Show)

data ListFollowedArtistsResponse = ListFollowedArtistsResponse
  { artists :: Maybe ArtistsObject
  }
  deriving (Generic, Show)

instance FromJSON ArtistsObject
instance FromJSON Cursor
instance FromJSON ListFollowedArtistsResponse
instance ToJSON ArtistsObject
instance ToJSON Cursor
instance ToJSON ListFollowedArtistsResponse
