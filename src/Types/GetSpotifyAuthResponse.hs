{-# LANGUAGE DeriveGeneric #-}

module Types.GetSpotifyAuthResponse where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data GetSpotifyAuthResponse = GetSpotifyAuthResponse
  { accessToken :: Text
  , refreshToken :: Text
  , expiresIn :: Int
  }
  deriving (Generic)

instance ToJSON GetSpotifyAuthResponse
