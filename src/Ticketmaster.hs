{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Ticketmaster
  ( searchEvents
  )
where

import Control.Monad (when)
import Data.Aeson
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale, FormatTime)
import Errors (throwErr, eitherStatusIO, mapLeft)
import HTTP (queryParam)
import Network.HTTP.Simple
import Network.HTTP.Types.Status (status500)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Types.Ticketmaster.SearchEventsRequest as SearchEventsRequest
import qualified Types.Ticketmaster.SearchEventsResponse as SearchEventsResponse

baseURL :: String
baseURL = "https://app.ticketmaster.com/discovery/v2"

searchEvents :: Text -> SearchEventsRequest.SearchEventsRequest -> IO SearchEventsResponse.SearchEventsResponse
searchEvents apiKey req = ticketmasterGet "/events.json" $
  [ ("apikey",        queryParam apiKey)
  , ("geoPoint",      queryParam . SearchEventsRequest.geoHash $ req)
  , ("radius",        queryParam . SearchEventsRequest.radiusMiles $ req)
  , ("unit",          queryParam ("miles" :: Text))
  , ("startDateTime", queryParam . timeString . SearchEventsRequest.startTime $ req)
  , ("endDateTime",   queryParam . timeString . SearchEventsRequest.endTime $ req)
  , ("size",          queryParam . SearchEventsRequest.pageSize $ req)
  , ("page",          queryParam . SearchEventsRequest.pageNumber $ req)
  ] <>
  map (("classificationName",) . queryParam) (SearchEventsRequest.classificationName req)

timeString :: FormatTime t => t -> String
timeString = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

ticketmasterGet :: FromJSON r => String -> Query -> IO r
ticketmasterGet path query = do
  let requestWithHeaders = setRequestQueryString query $ parseRequest_ (baseURL ++ path)
  response <- httpJSONEither requestWithHeaders

  when ((getResponseStatusCode response) /= 200) $
    throwErr (getResponseStatus response) ("failed to get" ++ path)

  eitherStatusIO status500 $ mapLeft show $ getResponseBody response
