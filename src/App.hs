{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module App
  ( AppState(..),
    App,
  )
where

import Control.Monad.Reader (ReaderT)
import Data.Text (Text)
import qualified Spotify

data AppState = AppState
  { spotifyCredentials :: Spotify.ClientCredentials
  , spotifyClientID :: Text
  , spotifyClientSecret :: Text
  , ticketmasterConsumerKey :: Text
  , ticketmasterConsumerSecret :: Text
  }
  deriving (Show)

type App = ReaderT AppState IO
