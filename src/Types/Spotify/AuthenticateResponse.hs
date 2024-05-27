{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Types.Spotify.AuthenticateResponse
  ( AuthenticateResponse (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Types.Spotify.Track as Track

data AuthenticateResponse = AuthenticateResponse
  { access_token :: String,
    refresh_token :: String,
    expires_in :: Int,
    scope :: String
  }
  deriving (Generic, Show)

instance FromJSON AuthenticateResponse

instance ToJSON AuthenticateResponse
