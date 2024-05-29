{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types.Ticketmaster.ListArtistsResponse
  ( ListArtistsResponse (..),
    fromEvents,
  )
where

import Data.Aeson (ToJSON)
import Data.List (nub)
import Data.Sort (sort)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Types.Ticketmaster.Attraction as Attraction
import qualified Types.Ticketmaster.Event as Event
import Data.Maybe (mapMaybe)

data ListArtistsResponse = ListArtistsResponse
  { nextPageToken :: Maybe Text,
    artists :: [Text]
  }
  deriving (Generic, Show)

instance ToJSON ListArtistsResponse

attractionNamesFromEvent :: Event.Event -> [Text]
attractionNamesFromEvent = maybe [] (mapMaybe Attraction.name) . Event.attractions . Event._embedded

fromEvents :: [Event.Event] -> ListArtistsResponse
fromEvents r =
  ListArtistsResponse
    { nextPageToken = Nothing,
      artists = nub . sort . concatMap attractionNamesFromEvent $ r
    }
