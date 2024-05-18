{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module App
  ( AppState(..),
    App,
  )
where

import Control.Monad.Reader (ReaderT)
import Data.Text (Text)
import qualified Spotify
import qualified Locations

data AppState = AppState
  { spotifyClientID :: Text
  , spotifyClientSecret :: Text
  , ticketmasterConsumerKey :: Text
  , ticketmasterConsumerSecret :: Text
  , postalCodeLookup :: Locations.PostalCodeLookup
  }
  deriving (Show)

type App = ReaderT AppState IO
