{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types.Ticketmaster.ListArtistsResponse
  ( ListArtistsResponse(..)
  , fromSearchEventsResponse
  )
  where

import Data.Maybe (fromMaybe)
import Data.Aeson
import Data.List (nub)
import Data.Sort (sort)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import qualified Types.Ticketmaster.Attraction as Attraction
import qualified Types.Ticketmaster.Event as Event
import qualified Types.Ticketmaster.SearchEventsResponse as SearchEventsResponse

data ListArtistsResponse = ListArtistsResponse
  { nextPageToken :: Maybe Text
  , artists :: [Text]
  }
  deriving (Generic, Show)

instance ToJSON ListArtistsResponse

attractionNamesFromEvent :: Event.Event -> [Text]
attractionNamesFromEvent = map Attraction.name . fromMaybe [] . Event.attractions . Event._embedded

attractionNamesFromResponse :: SearchEventsResponse.SearchEventsResponse -> [Text]
attractionNamesFromResponse = concatMap attractionNamesFromEvent . fromMaybe [] . SearchEventsResponse.events . SearchEventsResponse._embedded

nextPageNumber :: SearchEventsResponse.SearchEventsResponse -> Maybe Int
nextPageNumber r
  | pageNumber + 1 == totalPages = Nothing
  | otherwise = Just $ pageNumber + 1
  where
    pageNumber = SearchEventsResponse.number . SearchEventsResponse.page $ r
    totalPages = SearchEventsResponse.totalPages . SearchEventsResponse.page $ r

fromSearchEventsResponse :: SearchEventsResponse.SearchEventsResponse -> ListArtistsResponse
fromSearchEventsResponse r = ListArtistsResponse
  { nextPageToken = fmap (T.pack . show) . nextPageNumber $ r
  , artists = nub . sort . attractionNamesFromResponse $ r
  }
