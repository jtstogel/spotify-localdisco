{-# LANGUAGE DeriveGeneric #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Types.AuthenticateWithSpotifyRequest
  ( AuthenticateWithSpotifyRequest (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import GHC.Generics (Generic)

data AuthenticateWithSpotifyRequest = AuthenticateWithSpotifyRequest
  { code :: Text,
    redirectUri :: Text
  }
  deriving (Generic, Show, Eq)

instance FromJSON AuthenticateWithSpotifyRequest

instance ToJSON AuthenticateWithSpotifyRequest
