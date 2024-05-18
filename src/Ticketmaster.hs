{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Ticketmaster
  ( searchEvents
  )
where

import Control.Monad (when)
import Data.Aeson
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Format (formatTime, defaultTimeLocale, FormatTime)
import Data.Time.Clock (UTCTime)
import Network.HTTP.Simple
import Errors (throwErr, eitherStatusIO, mapLeft)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Types.Ticketmaster.SearchEventsRequest as SearchEventsRequest
import qualified Types.Ticketmaster.SearchEventsResponse as SearchEventsResponse
import Network.HTTP.Types.Status (status500)

baseURL :: String
baseURL = "https://app.ticketmaster.com/discovery/v2"

class QueryParam a where
  qp :: a -> Maybe B.ByteString

instance QueryParam String where
  qp = qp . T.pack

instance QueryParam Text where
  qp = Just . encodeUtf8

instance QueryParam Int where
  qp = qp . show

instance QueryParam UTCTime where
  qp = qp . timeString

searchEvents :: Text -> SearchEventsRequest.SearchEventsRequest -> IO SearchEventsResponse.SearchEventsResponse
searchEvents apiKey req = ticketmasterGet "/events.json" $
  [ ("apikey",        qp apiKey)
  , ("geoPoint",      qp . SearchEventsRequest.geoHash $ req)
  , ("radius",        qp . SearchEventsRequest.radiusMiles $ req)
  , ("unit",          qp ("miles" :: Text))
  , ("startDateTime", qp . SearchEventsRequest.startTime $ req)
  , ("endDateTime",   qp . SearchEventsRequest.endTime $ req)
  , ("size",          qp . SearchEventsRequest.pageSize $ req)
  , ("page",          qp . SearchEventsRequest.pageNumber $ req)
  ] <>
  map (("classificationName",) . qp) (SearchEventsRequest.classificationName req)

timeString :: FormatTime t => t -> String
timeString = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

ticketmasterGet :: FromJSON r => String -> Query -> IO r
ticketmasterGet path query = do
  let requestWithHeaders = setRequestQueryString query $ parseRequest_ (baseURL ++ path)
  response <- httpJSONEither requestWithHeaders

  when ((getResponseStatusCode response) /= 200) $
    throwErr (getResponseStatus response) "failed to search events"

  eitherStatusIO status500 $ mapLeft show $ getResponseBody response
