{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Spotify
    ( AccessToken(..)
    , getAccessToken
    ) where

import Network.HTTP.Simple
import Network.HTTP.Conduit (urlEncodedBody)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

baseURL :: String
baseURL = "https://api.spotify.com/v1/"

authURL :: String
authURL = "https://accounts.spotify.com/api/token"

data AccessToken = AccessToken Text deriving (Show)

data AccessTokenResponse = AccessTokenResponse
    { access_token :: Text
    } deriving (Show)

instance FromJSON AccessTokenResponse where
    parseJSON (Object v) = AccessTokenResponse
        <$> v .: "access_token"

getAccessToken :: Text -> Text -> IO AccessToken
getAccessToken clientID secret = do
    let requestBody = [("grant_type", "client_credentials")]
    let requestWithHeaders = setRequestBasicAuth (encodeUtf8 clientID) (encodeUtf8 secret) . urlEncodedBody requestBody . parseRequest_ $ authURL
    response <- httpJSON requestWithHeaders :: IO (Response AccessTokenResponse)
    return $ AccessToken $ access_token $ getResponseBody response

