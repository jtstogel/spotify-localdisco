{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Spotify
  ( getRecommendations,
    listTopTracks,
    listTopArtists,
    listFollowedArtists,
    listSavedTracks,
    getUserProfile,
    exchangeAuthCode,
  )
where

import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Debug.Trace (traceShowId)
import GHC.Generics (Generic)
import HTTP (get, queryParam)
import Network.HTTP.Conduit (urlEncodedBody)
import Network.HTTP.Simple
  ( Query,
    Response,
    getResponseBody,
    httpJSON,
    parseRequest_,
    setRequestBasicAuth,
    setRequestBearerAuth,
    setRequestQueryString,
  )
import qualified Types.Spotify.AuthenticateResponse as AuthenticateResponse
import qualified Types.Spotify.GetRecommendationsRequest as GetRecommendationsRequest
import qualified Types.Spotify.GetRecommendationsResponse as GetRecommendationsResponse
import qualified Types.Spotify.ListFollowedArtistsRequest as ListFollowedArtistsRequest
import qualified Types.Spotify.ListFollowedArtistsResponse as ListFollowedArtistsResponse
import qualified Types.Spotify.ListSavedTracksRequest as ListSavedTracksRequest
import qualified Types.Spotify.ListSavedTracksResponse as ListSavedTracksResponse
import qualified Types.Spotify.ListTopItemsRequest as ListTopItemsRequest
import qualified Types.Spotify.TopArtistsResponse as TopArtistsResponse
import qualified Types.Spotify.TopTracksResponse as TopTracksResponse
import qualified Types.Spotify.UserProfile as UserProfile

baseURL :: String
baseURL = "https://api.spotify.com/v1"

authURL :: String
authURL = "https://accounts.spotify.com/api/token"

exchangeAuthCode :: Text -> Text -> Text -> Text -> IO AuthenticateResponse.AuthenticateResponse
exchangeAuthCode clientID clientSecret redirectUri code = do
  let requestBody = [("grant_type", "authorization_code"), ("code", encodeUtf8 code), ("redirect_uri", encodeUtf8 redirectUri)]
  let requestWithHeaders = setRequestBasicAuth (encodeUtf8 clientID) (encodeUtf8 clientSecret) . urlEncodedBody requestBody . parseRequest_ $ authURL
  response <- httpJSON requestWithHeaders :: IO (Response AuthenticateResponse.AuthenticateResponse)
  return $ getResponseBody response

removeNothings :: Query -> Query
removeNothings = filter (isJust . snd)

topItemsQuery :: ListTopItemsRequest.ListTopItemsRequest -> Query
topItemsQuery req =
  [ ("limit", queryParam $ ListTopItemsRequest.limit req),
    ("offset", queryParam $ ListTopItemsRequest.offset req),
    ("time_range", queryParam $ ListTopItemsRequest.timeRange req)
  ]

listTopArtists :: Text -> ListTopItemsRequest.ListTopItemsRequest -> IO TopArtistsResponse.TopArtistsResponse
listTopArtists auth req = spotifyGet auth "/me/top/artists" (topItemsQuery req)

listTopTracks :: Text -> ListTopItemsRequest.ListTopItemsRequest -> IO TopTracksResponse.TopTracksResponse
listTopTracks auth req = spotifyGet auth "/me/top/tracks" (topItemsQuery req)

listFollowedArtists :: Text -> ListFollowedArtistsRequest.ListFollowedArtistsRequest -> IO ListFollowedArtistsResponse.ListFollowedArtistsResponse
listFollowedArtists auth req =
  spotifyGet auth "/me/following" $
    removeNothings
      [ ("type", Just "artist"),
        ("limit", queryParam . ListFollowedArtistsRequest.limit $ req),
        ("after", queryParam . ListFollowedArtistsRequest.after $ req)
      ]

listSavedTracks :: Text -> ListSavedTracksRequest.ListSavedTracksRequest -> IO ListSavedTracksResponse.ListSavedTracksResponse
listSavedTracks auth req =
  spotifyGet
    auth
    "/me/tracks"
    [ ("limit", queryParam . ListSavedTracksRequest.limit $ req),
      ("offset", queryParam . ListSavedTracksRequest.offset $ req)
    ]

getRecommendations :: Text -> GetRecommendationsRequest.GetRecommendationsRequest -> IO GetRecommendationsResponse.GetRecommendationsResponse
getRecommendations auth req = do
  -- Spotify does not like lots of requests to `/recommendations`.
  threadDelay 1000000
  spotifyGet
    auth
    "/recommendations"
    [ ("limit", queryParam . GetRecommendationsRequest.limit $ traceShowId req),
      ("seed_artists", queryParam . T.intercalate "," . GetRecommendationsRequest.seedArtists $ req),
      ("seed_tracks", queryParam . T.intercalate "," . GetRecommendationsRequest.seedTracks $ req)
    ]

getUserProfile :: Text -> IO UserProfile.UserProfile
getUserProfile auth = spotifyGet auth "/me" []

spotifyGet :: (FromJSON r, Show r) => Text -> String -> Query -> IO r
spotifyGet token path query =
  get
    . setRequestQueryString (removeNothings query)
    . setRequestBearerAuth (encodeUtf8 token)
    . parseRequest_
    $ baseURL ++ path
