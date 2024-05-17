{-# LANGUAGE DeriveGeneric #-}

module Types.Ticketmaster.SearchEventsRequest where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

-- https://app.ticketmaster.com/discovery/v2/events.json?apikey=9vhnklhpQwMKDWAcuMwTQAfXh57gAMXy&postalCode=94110&radius=10&unit=miles&page=0&size=100&classificationName=music

data DistanceUnit = Miles | Kilometers

data SearchEventsRequest = SearchEventsRequest
  { geoHash :: Text              -- 
  , radius :: Int                -- Search radius around geoHash (use with `unit`)
  , unit :: DistanceUnit         -- Unit for radius.
  , startDateTime :: Text        -- RFC3339 start date
  , endDateTime :: Text          -- RFC3339 end date
  , size :: Int                  -- Page size
  , page :: Int                  -- Page number
  , classificationName :: [Text] -- Event classification (eg "music")
  , apikey :: Text               -- Client API key
  }
