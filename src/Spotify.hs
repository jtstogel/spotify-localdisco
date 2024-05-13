{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Spotify
  ( AuthorizationToken(..),
    ClientCredentials,
    getAuthToken,
    getClientCredentials,
    getTopArtists,
    getUserProfile,
  )
where

import Data.Aeson
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Network.HTTP.Conduit (urlEncodedBody)
import Network.HTTP.Simple
import Data.Maybe (fromJust)
import qualified Types.Spotify.TopItems as TopItems
import qualified Types.Spotify.UserProfile as UserProfile

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

data ClientCredentials = ClientCredentials Text deriving (Show)

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

getAuthToken :: Text -> Text -> Text -> Text -> IO AuthorizationToken
getAuthToken clientID clientSecret code redirectURI = do
  let requestBody =
        [ ("grant_type", "authorization_code"),
          ("redirect_uri", encodeUtf8 redirectURI),
          ("code", encodeUtf8 code)
        ]
  let requestWithHeaders = setRequestBasicAuth (encodeUtf8 clientID) (encodeUtf8 clientSecret) . urlEncodedBody requestBody . parseRequest_ $ authURL
  response <- httpJSON requestWithHeaders :: IO (Response AccessTokenResponse)
  let body = getResponseBody response

  return $
    AuthorizationToken
      { authorizationToken = access_token body,
        refreshToken = fromJust $ refresh_token body,
        expiresIn = expires_in body
      }

getTopArtists :: Text -> IO TopItems.TopItemsResponse
getTopArtists auth = spotifyGet auth "/me/top/artists" [("limit", Just "50")]

getUserProfile :: Text -> IO UserProfile.UserProfileResponse
getUserProfile creds = spotifyGet creds "/me" []

spotifyGet :: FromJSON r => Text -> String -> Query -> IO r
spotifyGet token path query = do
  let requestWithHeaders = setRequestQueryString query $ setRequestBearerAuth (encodeUtf8 token) $ parseRequest_ (baseURL ++ path)
  response <- httpJSON requestWithHeaders
  return $ getResponseBody response
