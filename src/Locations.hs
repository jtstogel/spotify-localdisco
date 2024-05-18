{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Locations
  ( buildPostalCodeLookup
  , lookupGeoHash
  , LatLon(..)
  , PostalCodeLookup(..)
  , PostalCodeLocation(..)
  )
where

import Data.Aeson
import Data.Text (Text)
import Errors (eitherFromMaybe)
import GHC.Generics
import qualified Data.Map as M
import qualified GeoHash as G

data PostalCodeLocation = PostalCodeLocation
    { postalCode :: Text
    , coordinate :: LatLon
    }
    deriving (Generic)

instance FromJSON PostalCodeLocation

data LatLon = LatLon
    { lat :: Double
    , lon :: Double
    }
    deriving (Generic, Show)

instance FromJSON LatLon

data PostalCodeLookup = PostalCodeLookup
    { lookupEntries :: M.Map Text LatLon
    }
    deriving (Show)

postalCodeMap :: [PostalCodeLocation] -> M.Map Text LatLon
postalCodeMap = M.fromList . map (\e -> (postalCode e, coordinate e))

buildPostalCodeLookup :: [PostalCodeLocation] -> PostalCodeLookup
buildPostalCodeLookup entries = PostalCodeLookup { lookupEntries = postalCodeMap entries }

lookupGeoHash :: PostalCodeLookup -> Text -> Either String String
lookupGeoHash postalLookup code = do
    latLon <- eitherFromMaybe "postal code not found" $ M.lookup code (lookupEntries postalLookup)
    return $ G.geoHash (lat latLon, lon latLon)
