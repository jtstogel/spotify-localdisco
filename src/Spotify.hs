{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Spotify
  ( AuthorizationToken(..)
  , ClientCredentials
  , getClientCredentials
  , getRecommendations
  , listTopTracks
  , listTopArtists
  , listFollowedArtists
  , listSavedTracks
  )
where

import Control.Monad (when)
import Data.Aeson
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Debug.Trace
import Errors (throwErr, eitherStatusIO, mapLeft)
import GHC.Generics
import HTTP (queryParam)
import Network.HTTP.Conduit (urlEncodedBody)
import Network.HTTP.Simple
import Network.HTTP.Types.Status (status500)
import qualified Data.Text as T
import qualified Types.Spotify.GetRecommendationsRequest as GetRecommendationsRequest
import qualified Types.Spotify.GetRecommendationsResponse as GetRecommendationsResponse
import qualified Types.Spotify.ListFollowedArtistsRequest as ListFollowedArtistsRequest
import qualified Types.Spotify.ListFollowedArtistsResponse as ListFollowedArtistsResponse
import qualified Types.Spotify.ListSavedTracksRequest as ListSavedTracksRequest
import qualified Types.Spotify.ListSavedTracksResponse as ListSavedTracksResponse
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

removeNothings :: Query -> Query
removeNothings = filter (isJust . snd)

topItemsQuery :: ListTopItemsRequest.ListTopItemsRequest -> Query
topItemsQuery req =
  [ ("limit",      queryParam $ ListTopItemsRequest.limit req)
  , ("offset",     queryParam $ ListTopItemsRequest.offset req)
  , ("time_range", queryParam $ ListTopItemsRequest.timeRange req)
  ]

listTopArtists :: Text -> ListTopItemsRequest.ListTopItemsRequest -> IO TopArtistsResponse.TopArtistsResponse
listTopArtists auth req = spotifyGet auth "/me/top/artists" $ removeNothings (topItemsQuery req)

listTopTracks :: Text -> ListTopItemsRequest.ListTopItemsRequest -> IO TopTracksResponse.TopTracksResponse
listTopTracks auth req = spotifyGet auth "/me/top/tracks" $ removeNothings (topItemsQuery req)

listFollowedArtists :: Text -> ListFollowedArtistsRequest.ListFollowedArtistsRequest -> IO ListFollowedArtistsResponse.ListFollowedArtistsResponse
listFollowedArtists auth req = spotifyGet auth "/me/following" $ removeNothings
  [ ("type",  Just "artist")
  , ("limit", queryParam . ListFollowedArtistsRequest.limit $ req)
  , ("after", queryParam . ListFollowedArtistsRequest.after $ req)
  ]

listSavedTracks :: Text -> ListSavedTracksRequest.ListSavedTracksRequest -> IO ListSavedTracksResponse.ListSavedTracksResponse
listSavedTracks auth req = spotifyGet auth "/me/tracks" $ removeNothings
  [ ("limit",  queryParam . ListSavedTracksRequest.limit $ req)
  , ("offset", queryParam . ListSavedTracksRequest.offset $ req)
  ]

getRecommendations :: Text -> GetRecommendationsRequest.GetRecommendationsRequest -> IO GetRecommendationsResponse.GetRecommendationsResponse
getRecommendations auth req = spotifyGet auth "/recommendations" $ removeNothings
  [ ("limit",        queryParam . GetRecommendationsRequest.limit $ (traceShowId req))
  , ("seed_artists", queryParam . T.intercalate "," . GetRecommendationsRequest.seedArtists $ req)
  , ("seed_tracks",  queryParam . T.intercalate "," . GetRecommendationsRequest.seedTracks $ req)
  ]

spotifyGet :: (FromJSON r, Show r) => Text -> String -> Query -> IO r
spotifyGet token path query = do
  let requestWithHeaders = setRequestQueryString (traceShowId query) $ setRequestBearerAuth (encodeUtf8 token) $ parseRequest_ (baseURL ++ path)
  response <- httpJSONEither (traceShowId requestWithHeaders)

  when ((getResponseStatusCode (traceShowId response)) /= 200) $
    throwErr (getResponseStatus response) ("failed to get " ++ path)

  return $ getResponseBody response

  eitherStatusIO status500 $ mapLeft show $ getResponseBody response
