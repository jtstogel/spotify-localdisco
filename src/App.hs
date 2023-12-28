module App
    ( AppState(..)
    , App
    ) where

import qualified Spotify
import Control.Monad.Reader (ReaderT)


data AppState = AppState
  { spotifyAuth :: Spotify.ClientCredentials
  } deriving (Show)

type App = ReaderT AppState IO
