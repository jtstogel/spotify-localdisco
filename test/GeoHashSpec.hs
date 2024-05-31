{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module GeoHashSpec (spec) where

import Data.List (isPrefixOf)
import Data.Ratio ((%))
import GeoHash (geoHash)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

eps :: Rational
eps = 1 % 10000000

spec :: Spec
spec = do
  describe "geoHash" $ do
    it "should compute typical examples" $ do
      -- Skagen, Denmark
      geoHash ((57.64911, 10.40744) :: (Double, Double)) `shouldBe` "u4pruydqqvj8"
      -- Oshkosh, Wisconsin, USA. Reduced precision from Float means we only get a prefix.
      geoHash ((44.022568, -88.553099) :: (Float, Float)) `shouldSatisfy` isPrefixOf "dpc42yjd"
      -- Mission Dolores Park
      geoHash (37.76001572, -122.42731774) `shouldBe` "9q8yy450h9p7"

    it "should compute near boundaries" $ do
      geoHash (90 - eps, 180 - eps) `shouldBe` "zzzzzzzzzzzz"
      geoHash (eps - 90, eps - 180) `shouldBe` "000000000000"
      geoHash (eps - 90, 180 - eps) `shouldBe` "pbpbpbpbpbpb"
      geoHash (90 - eps, eps - 180) `shouldBe` "bpbpbpbpbpbp"
