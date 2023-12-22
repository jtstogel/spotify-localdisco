module Main (main) where

import qualified Env
import qualified Spotify

main :: IO ()
main = do
    env <- Env.load ".env"
    putStrLn $ show env

    token <- Spotify.getAccessToken (Env.spotifyClientID env) (Env.spotifyClientSecret env)

    putStrLn $ show token
