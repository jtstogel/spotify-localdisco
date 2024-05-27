{-# LANGUAGE DeriveGeneric #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Types.AuthenticateWithSpotifyResponse
  ( AuthenticateWithSpotifyResponse (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import GHC.Generics (Generic)

data AuthenticateWithSpotifyResponse = AuthenticateWithSpotifyResponse
  -- The user's ID with Local Disco.
  -- Knowledge of the ID is proof of the user's identity.
  { userId :: !Text
  }
  deriving (Generic, Show, Eq)

instance FromJSON AuthenticateWithSpotifyResponse

instance ToJSON AuthenticateWithSpotifyResponse
