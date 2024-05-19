{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types.Ticketmaster.ListArtistsResponse
  ( ListArtistsResponse(..)
  , fromEvents
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

fromEvents :: [Event.Event] -> ListArtistsResponse
fromEvents r = ListArtistsResponse
  { nextPageToken = Nothing
  , artists = nub . sort . concatMap attractionNamesFromEvent $ r
  }
