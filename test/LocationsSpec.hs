{-# LANGUAGE OverloadedStrings #-}

module LocationsSpec (spec) where

import Locations
import Test.Hspec

spec :: Spec
spec = do
  describe "postal lookup" $ do
    let postalLookup = Locations.buildPostalCodeLookup [ PostalCodeLocation
            { postalCode = "94110"
            , coordinate = LatLon
                { lat =  37.76001572
                , lon = -122.42731774
                }
            }]

    it "should error if postal code does not exist" $ do
      lookupGeoHash postalLookup "does not exist" `shouldBe` (Left "postal code not found")

    it "should return geo hash on successful lookup" $ do
      lookupGeoHash postalLookup "94110" `shouldBe` (Right "9q8yy450h9p7")
