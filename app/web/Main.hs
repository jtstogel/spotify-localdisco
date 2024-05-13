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
import qualified Types.GetSpotifyAuthRequest as GetSpotifyAuthRequest
import qualified Types.GetSpotifyAuthResponse as GetSpotifyAuthResponse
import qualified Types.GetSpotifyClientIdResponse as GetSpotifyClientIdResponse
import qualified Types.GetSpotifyUserProfileResponse as GetSpotifyUserProfileResponse
import qualified Types.Spotify.UserProfile as SpotifyUserProfile
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
          { App.spotifyCredentials = credentials,
            App.spotifyClientID = Env.spotifyClientID env,
            App.spotifyClientSecret = Env.spotifyClientSecret env
          }

  routes appState

getSpotifyAuth :: App.AppState -> ActionM ()
getSpotifyAuth appState = do
  req <- S.jsonData :: ActionM GetSpotifyAuthRequest.GetSpotifyAuthRequest
  let code = GetSpotifyAuthRequest.code req
  let redirectUri = GetSpotifyAuthRequest.redirectUri req

  token <- liftIO $ Spotify.getAuthToken (App.spotifyClientID appState) (App.spotifyClientSecret appState) code redirectUri

  S.json $
    GetSpotifyAuthResponse.GetSpotifyAuthResponse
      { GetSpotifyAuthResponse.accessToken = Spotify.authorizationToken token
      , GetSpotifyAuthResponse.refreshToken = Spotify.refreshToken token
      , GetSpotifyAuthResponse.expiresIn = Spotify.expiresIn token
      }

getSpotifyClientId :: App.AppState -> ActionM ()
getSpotifyClientId appState = do
  S.json $
    GetSpotifyClientIdResponse.GetSpotifyClientIdResponse
      { GetSpotifyClientIdResponse.clientId = App.spotifyClientID appState
      }

getSpotifyUserProfile :: App.AppState -> ActionM ()
getSpotifyUserProfile appState = do
  accessToken <- S.queryParam "accessToken" :: ActionM Text

  profile <- liftIO $ Spotify.getUserProfile accessToken
  let imageUrl = SpotifyUserProfile.url <$> maybeHead (SpotifyUserProfile.images profile)

  S.json $
    GetSpotifyUserProfileResponse.GetSpotifyUserProfileResponse
      { GetSpotifyUserProfileResponse.displayName = SpotifyUserProfile.display_name profile,
        GetSpotifyUserProfileResponse.profileImageUrl = imageUrl
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
        }

routes :: App.AppState -> IO ()
routes appState = S.scotty 8080 $ do
  S.middleware allowCors
  S.post "/spotify/authenticate" $ getSpotifyAuth appState
  S.get "/spotify/clientId" $ getSpotifyClientId appState
  S.get "/spotify/users/me" $ getSpotifyUserProfile appState
