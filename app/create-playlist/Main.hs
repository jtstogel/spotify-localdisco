module Main (main) where

import qualified Env
import qualified Spotify
import Control.Monad.Reader (runReaderT, asks)
import App (App, AppState(..))


program :: App String
program = do
    tok <- asks spotifyCredentials
    return . show $ tok

main :: IO ()
main = do
    env <- Env.load ".env"
    print env

    token <- Spotify.getClientCredentials (Env.spotifyClientID env) (Env.spotifyClientSecret env)
    let appState = AppState { spotifyCredentials = token }
    value <- runReaderT program appState
    putStrLn value
