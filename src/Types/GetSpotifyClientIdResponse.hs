{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Types.GetSpotifyClientIdResponse
  ( GetSpotifyClientIdResponse(..),
  )
where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data GetSpotifyClientIdResponse = GetSpotifyClientIdResponse
  { clientId :: !Text
  }
  deriving (Generic)

instance ToJSON GetSpotifyClientIdResponse
