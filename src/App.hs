module App
    ( AppState(..)
    ) where

import qualified Spotify

data AppState = AppState
  { spotifyAuth :: Spotify.AccessToken
  } deriving (Show)

