module Main (main) where

import qualified Env
import qualified Spotify
import Control.Monad.Reader (runReaderT, ask)
import App (App, AppState(..))


program :: App String
program = do
    tok <- spotifyAuth <$> ask
    return . show $ tok

main :: IO ()
main = do
    env <- Env.load ".env"
    putStrLn $ show env

    token <- Spotify.getAccessToken (Env.spotifyClientID env) (Env.spotifyClientSecret env)
    let appState = AppState { spotifyAuth = token }
    value <- runReaderT program appState
    putStrLn value
