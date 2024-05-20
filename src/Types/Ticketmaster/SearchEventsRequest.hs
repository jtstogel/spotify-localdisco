module Types.Ticketmaster.SearchEventsRequest
  ( SearchEventsRequest(..)
  )
  where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)

data SearchEventsRequest = SearchEventsRequest
  { geoHash :: !Text              -- Location to search near. Maximum 9 characters.
  , radiusMiles :: !Int           -- Search radius around geoHash.
  , classificationName :: ![Text] -- Event classification (eg "music")
  , startTime :: !UTCTime
  , endTime :: !UTCTime
  , pageSize :: !Int
  , pageNumber :: !Int
  }
