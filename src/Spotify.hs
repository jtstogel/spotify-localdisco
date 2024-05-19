{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Spotify
  ( AuthorizationToken(..)
  , ClientCredentials
  , getClientCredentials
  , listTopTracks
  , listTopArtists
  )
where

import Control.Monad (when)
import Data.Aeson
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Errors (throwErr)
import GHC.Generics
import HTTP (queryParam)
import Network.HTTP.Conduit (urlEncodedBody)
import Network.HTTP.Simple
import Network.HTTP.Types.Status (status500)
import qualified Types.Spotify.ListTopItemsRequest as ListTopItemsRequest
import qualified Types.Spotify.TopArtistsResponse as TopArtistsResponse
import qualified Types.Spotify.TopTracksResponse as TopTracksResponse

baseURL :: String
baseURL = "https://api.spotify.com/v1"

authURL :: String
authURL = "https://accounts.spotify.com/api/token"

data AuthorizationToken = AuthorizationToken
  { authorizationToken :: Text,
    refreshToken :: Text,
    expiresIn :: Int
  }
  deriving (Generic, Show)

newtype ClientCredentials = ClientCredentials Text deriving (Show)

data AccessTokenResponse = AccessTokenResponse
  { access_token :: Text,
    expires_in :: Int,
    refresh_token :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON AccessTokenResponse

getClientCredentials :: Text -> Text -> IO ClientCredentials
getClientCredentials clientID secret = do
  let requestBody = [("grant_type", "client_credentials")]
  let requestWithHeaders = setRequestBasicAuth (encodeUtf8 clientID) (encodeUtf8 secret) . urlEncodedBody requestBody . parseRequest_ $ authURL
  response <- httpJSON requestWithHeaders :: IO (Response AccessTokenResponse)
  return $ ClientCredentials $ access_token $ getResponseBody response

topItemsQuery :: ListTopItemsRequest.ListTopItemsRequest -> Query
topItemsQuery req =
  [ ("limit",      (>>= queryParam) $ ListTopItemsRequest.limit req)
  , ("offset",     (>>= queryParam) $ ListTopItemsRequest.offset req)
  , ("time_range", (>>= queryParam) $ ListTopItemsRequest.timeRange req)
  ]

listTopArtists :: Text -> ListTopItemsRequest.ListTopItemsRequest -> IO TopArtistsResponse.TopArtistsResponse
listTopArtists auth req = spotifyGet auth "/me/top/artists" (topItemsQuery req)

listTopTracks :: Text -> ListTopItemsRequest.ListTopItemsRequest -> IO TopTracksResponse.TopTracksResponse
listTopTracks auth req = spotifyGet auth "/me/top/tracks" (topItemsQuery req)

spotifyGet :: FromJSON r => Text -> String -> Query -> IO r
spotifyGet token path query = do
  let requestWithHeaders = setRequestQueryString query $ setRequestBearerAuth (encodeUtf8 token) $ parseRequest_ (baseURL ++ path)
  response <- httpJSON requestWithHeaders

  when ((getResponseStatusCode response) /= 200) $
    throwErr (getResponseStatus response) ("failed to get" ++ path)

  return $ getResponseBody response
