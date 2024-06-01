{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Ticketmaster
  ( searchEvents,
  )
where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format (FormatTime, defaultTimeLocale, formatTime)
import HTTP (get, queryParam)
import Network.HTTP.Simple
  ( Query,
    parseRequest_,
    setRequestQueryString,
  )
import qualified Types.Ticketmaster.SearchEventsRequest as SearchEventsRequest
import qualified Types.Ticketmaster.SearchEventsResponse as SearchEventsResponse

baseURL :: String
baseURL = "https://app.ticketmaster.com/discovery/v2"

searchEvents :: Text -> SearchEventsRequest.SearchEventsRequest -> IO SearchEventsResponse.SearchEventsResponse
searchEvents apiKey req =
  ticketmasterGet "/events.json" $
    [ ("apikey", queryParam apiKey),
      ("geoPoint", queryParam . T.take 9 . SearchEventsRequest.geoHash $ req),
      ("radius", queryParam . SearchEventsRequest.radiusMiles $ req),
      ("unit", queryParam ("miles" :: Text)),
      ("startDateTime", queryParam . timeString . SearchEventsRequest.startTime $ req),
      ("endDateTime", queryParam . timeString . SearchEventsRequest.endTime $ req),
      ("size", queryParam . SearchEventsRequest.pageSize $ req),
      ("page", queryParam . SearchEventsRequest.pageNumber $ req)
    ]
      <> map (("classificationName",) . queryParam) (SearchEventsRequest.classificationName req)

timeString :: (FormatTime t) => t -> String
timeString = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

ticketmasterGet :: (FromJSON r, Show r) => String -> Query -> IO r
ticketmasterGet path query =
  get
    . setRequestQueryString query
    . parseRequest_
    $ baseURL ++ path
