{-# LANGUAGE OverloadedStrings #-}
module Web.Shipping.Tracking.DetectionSpec (spec) where

import Data.ByteString (ByteString(..))
import Data.ByteString.Char8 ()
import Test.Hspec
import Test.Hspec.HUnit ()
import Test.HUnit

import Web.Shipping.Tracking.Detection
import Web.Shipping.Carriers

spec :: Spec
spec = do
  describe "detectCarrier USPS" $ do
    it "detects USPS certified" $
      (detectCarrier "70000000000000000000") ~?= Just USPS

    it "detects USPS certified with whitespace" $
      (detectCarrier "7000 0000 0000 0000 0000") ~?= Just USPS

    it "detects USPS delivery confirmation" $
      (detectCarrier "9405501548007115480421") ~?= Just USPS

    it "detects USPS delivery confirmation with spaces" $
      (detectCarrier "9405 5015 4800 7115 4804 21") ~?= Just USPS

    it "detects USPS tracking numbers beginning in 91" $
      (detectCarrier "9105501548007115480421") ~?= Just USPS

    it "detects USPS priority mail international" $
      (detectCarrier "CP000000000US") ~?= Just USPS

    it "detects USPS priority mail international with spaces" $
      (detectCarrier "CP 000 000 000 US") ~?= Just USPS

  describe "detectCarrier UPS" $ do
    it "detects 1Z tracking numbers" $
      (detectCarrier "1Z9999999999999999") ~?= Just UPS

  describe "detectCarrier misses" $ do
    it "does not detect things that aren't USPS" $
      (detectCarrier "wat") ~?= Nothing
