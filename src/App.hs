{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module App
  ( AppState (..),
    App,
  )
where

import Control.Monad.Reader (ReaderT)
import qualified Spotify

data AppState = AppState
  { spotifyAuth :: Spotify.ClientCredentials
  }
  deriving (Show)

type App = ReaderT AppState IO
