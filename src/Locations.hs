{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Locations
  ( buildPostalCodeLookup
  , lookupGeoHash
  , lookupPlaceName
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
    , latitude :: Double
    , longitude :: Double
    , placeName :: Text
    }
    deriving (Generic, Show)

instance FromJSON PostalCodeLocation

data PostalCodeLookup = PostalCodeLookup
    { lookupEntries :: M.Map Text PostalCodeLocation
    }
    deriving (Show)

buildPostalCodeLookup :: [PostalCodeLocation] -> PostalCodeLookup
buildPostalCodeLookup entries = PostalCodeLookup
  { lookupEntries = M.fromList . map (\e -> (postalCode e, e)) $ entries
  }

lookupEntry :: PostalCodeLookup -> Text -> Either String PostalCodeLocation
lookupEntry l code = eitherFromMaybe "postal code not found" . M.lookup code . lookupEntries $ l

locationGeoHash :: PostalCodeLocation -> String
locationGeoHash loc = G.geoHash (latitude loc, longitude loc)

lookupGeoHash :: PostalCodeLookup -> Text -> Either String String
lookupGeoHash postalCodeLookup code = locationGeoHash <$> lookupEntry postalCodeLookup code

lookupPlaceName :: PostalCodeLookup -> Text -> Either String Text
lookupPlaceName postalCodeLookup code = placeName <$> lookupEntry postalCodeLookup code
