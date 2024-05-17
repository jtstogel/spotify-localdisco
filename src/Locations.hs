{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Locations
  ( lookupGeoHash
  , load
  )
where

import Data.Text (Text)
import qualified Data.Map as M

data PostalCodeLocationEntry = PostalCodeLocationEntry
    { postalCode :: Text
    , lat :: Double
    , lon :: Double
    }

data LatLon = LatLon
    { latitude :: Double
    , longitude :: Double
    }

data PostalCodeLookup = PostalCodeLookup
    { entries :: M.Map Text LatLon
    }

load :: Text -> IO PostalCodeLookup
load dataJsonFile = return $ PostalCodeLookup { entries = M.empty }

lookupGeoHash :: PostalCodeLookup -> Text -> Text
lookupGeoHash translator postalCode = ""
