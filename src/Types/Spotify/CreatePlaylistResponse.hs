{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{- HLINT ignore "Use newtype instead of data" -}

module Types.Spotify.CreatePlaylistResponse
  ( CreatePlaylistResponse (..),
    ExternalUrls (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data ExternalUrls = ExternalUrls
  { spotify :: Text
  }
  deriving (Generic, Show)

data CreatePlaylistResponse = CreatePlaylistResponse
  { external_urls :: !ExternalUrls,
    id :: !Text
  }
  deriving (Generic, Show)

instance FromJSON CreatePlaylistResponse

instance ToJSON CreatePlaylistResponse

instance FromJSON ExternalUrls

instance ToJSON ExternalUrls
