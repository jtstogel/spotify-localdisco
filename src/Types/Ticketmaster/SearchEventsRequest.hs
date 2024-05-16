{-# LANGUAGE DeriveGeneric #-}

module Types.Ticketmaster.SearchEventsRequest where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data SearchEventsRequest = SearchEventsRequest
  -- Number of rows to return.
  { size :: Maybe Int
  -- Page number to fetch.
  , page :: Maybe Int
  -- Comma separated list of Category IDs (eg 10001 is Music).
  , category_ids :: Text
  -- Postal code around which to find events.
  , postalCode :: Text
  -- Radius in kilometers around postal_code.
  , radius :: Int
  -- Start date for search in RFC3339 format.
  , startDateTime :: Text
  -- Start date for search in RFC3339 format.
  , endDateTime :: Text
  }
  deriving (Generic)

instance FromJSON SearchEventsRequest
instance ToJSON SearchEventsRequest
