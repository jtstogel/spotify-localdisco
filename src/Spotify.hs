{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Spotify
    ( AuthorizationToken
    , ClientCredentials
    , getAccessToken
    , getTopArtists
    ) where

import GHC.Generics
import Network.HTTP.Simple
import Network.HTTP.Conduit (urlEncodedBody)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Char (toLower)


baseURL :: String
baseURL = "https://api.spotify.com/v1"

authURL :: String
authURL = "https://accounts.spotify.com/api/token"

data AuthorizationToken = AuthorizationToken
  { authorizationToken :: Text
  , refreshToken :: Text
  , expiryUnixSeconds :: Integer
  } deriving (Generic, Show)

data ClientCredentials = ClientCredentials Text deriving (Show)

data AccessTokenResponse = AccessTokenResponse
    { access_token :: Text
    , expires_in :: Int
    , refresh_token :: Maybe Text
    } deriving (Generic, Show)

instance FromJSON AccessTokenResponse

data Item = Item { id :: Text } deriving (Generic, Show)

instance FromJSON Item
 
data TopItemsResponse = TopItemsResponse
    { next :: Maybe Text
    , offset :: Maybe Int
    , items :: [Item]
    } deriving (Generic, Show)

instance FromJSON TopItemsResponse

getAccessToken :: Text -> Text -> IO ClientCredentials
getAccessToken clientID secret = do
    let requestBody = [("grant_type", "client_credentials")]
    let requestWithHeaders = setRequestBasicAuth (encodeUtf8 clientID) (encodeUtf8 secret) . urlEncodedBody requestBody . parseRequest_ $ authURL
    response <- httpJSON requestWithHeaders :: IO (Response AccessTokenResponse)
    return $ ClientCredentials $ access_token $ getResponseBody response

getTopArtists :: AuthorizationToken -> IO [Item]
getTopArtists AuthorizationToken{authorizationToken=token} = do
    let query = [("limit", Just "50")]
    let requestWithHeaders = setRequestQueryString query $ setRequestBearerAuth (encodeUtf8 token) $ parseRequest_ (baseURL ++ "/me/top/artists")
    response <- httpJSON requestWithHeaders :: IO (Response TopItemsResponse)
    return $ items $ getResponseBody response


-- getTopArtists = decode "{\"next\": \"pagetoken\", \"items\":[{\"id\":\"v\", \"type\":\"artist\"}]}"
