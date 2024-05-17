{-# LANGUAGE DeriveGeneric #-}

module Types.Ticketmaster.SearchEventsRequest where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

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
