{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import App (App)
import qualified App
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks, runReaderT)
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Env (Env (spotifyClientID))
import qualified Env
import qualified Spotify
import qualified Types.GetSpotifyClientIdResponse as GetSpotifyClientIdResponse
import Web.Scotty (ActionM)
import qualified Web.Scotty as S
import Network.Wai                       (Middleware)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Cors       (CorsResourcePolicy(..), cors, simpleCorsResourcePolicy)

main :: IO ()
main = do
  env <- Env.load ".env"
  print env

  credentials <- Spotify.getClientCredentials (Env.spotifyClientID env) (Env.spotifyClientSecret env)
  let appState =
        App.AppState
          { App.spotifyCredentials = credentials
          , App.spotifyClientID = Env.spotifyClientID env
          , App.spotifyClientSecret = Env.spotifyClientSecret env
          , App.ticketmasterConsumerKey = Env.ticketmasterConsumerKey env
          , App.ticketmasterConsumerSecret = Env.ticketmasterConsumerSecret env
          }

  routes appState

getSpotifyClientId :: App.AppState -> ActionM ()
getSpotifyClientId appState = do
  S.json $
    GetSpotifyClientIdResponse.GetSpotifyClientIdResponse
      { GetSpotifyClientIdResponse.clientId = App.spotifyClientID appState
      }

maybeHead :: [a] -> Maybe a
maybeHead (x:_) = Just x
maybeHead [] = Nothing

allowCors :: Middleware
allowCors = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
    simpleCorsResourcePolicy
        { corsMethods = ["OPTIONS", "GET", "PUT", "POST"]
        , corsRequestHeaders = ["content-type"]
        }

routes :: App.AppState -> IO ()
routes appState = S.scotty 8080 $ do
  S.middleware allowCors
  S.get "/spotify/clientId" $ getSpotifyClientId appState
