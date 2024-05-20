{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module App
  ( AppState (..),
    App,
  )
where

import Control.Monad.Reader (ReaderT)
import Data.Text (Text)
import qualified Jobs
import qualified Locations

data AppState = AppState
  { spotifyClientID :: !Text,
    spotifyClientSecret :: !Text,
    ticketmasterConsumerKey :: !Text,
    ticketmasterConsumerSecret :: !Text,
    postalCodeLookup :: !Locations.PostalCodeLookup,
    jobsDB :: !Jobs.DB
  }

type App = ReaderT AppState IO
