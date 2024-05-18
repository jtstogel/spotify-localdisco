{-# LANGUAGE OverloadedStrings #-}

module LocationsSpec (spec) where

import Locations
import Test.Hspec

spec :: Spec
spec = do
  describe "postal lookup" $ do
    let postalLookup = Locations.buildPostalCodeLookup [ PostalCodeLocation
            { postalCode = "94110"
            , latitude =  37.76001572
            , longitude = -122.42731774
            , placeName = "San Francisco"
            }]

    it "should error if postal code does not exist" $ do
      lookupGeoHash postalLookup "does not exist" `shouldBe` (Left "postal code not found")

    it "lookupGeoHash should succeed if postal code exists" $ do
      lookupGeoHash postalLookup "94110" `shouldBe` (Right "9q8yy450h9p7")

    it "lookupPlaceName should succeed if postal code exists" $ do
      lookupPlaceName postalLookup "94110" `shouldBe` (Right "San Francisco")
