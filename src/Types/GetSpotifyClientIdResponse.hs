{-# LANGUAGE DeriveGeneric #-}

module Types.GetSpotifyClientIdResponse where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data GetSpotifyClientIdResponse = GetSpotifyClientIdResponse
  { clientId :: !Text
  }
  deriving (Generic)

instance ToJSON GetSpotifyClientIdResponse
