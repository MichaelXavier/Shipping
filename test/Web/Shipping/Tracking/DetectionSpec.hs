{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Shipping.Tracking.DetectionSpec (spec) where

import SpecHelper

import Web.Shipping.Tracking.Detection
import Web.Shipping.Types
import Web.Shipping.Carriers

--TODO: swap these out for legit tracking numbers
spec :: Spec
spec = do
  describe "VerifiableTrackingNumber Instances" $ do
    describe "USPSTracking" $ do
      it "verifies the sample mod10 checksum in the docs" $
        verifyTrackingNumber (USPSTracking "71123456789123456787") `shouldBe` True

      it "verifies the sample mod11 checksum in the docs" $
        verifyTrackingNumber (USPSTracking "EF123456785US") `shouldBe` True

      it "is exception safe due to guards" $ property $ \(t :: USPSTracking) ->
        isExceptionSafe $ verifyTrackingNumber t

    describe "UPSTracking" $ do
      it "verifies a valid tracking number" $
        verifyTrackingNumber (UPSTracking "1Z6W20951326367397") `shouldBe` True
      -- it "is exception safe due to guards" $ property $ \(t :: UPSTracking) ->
      --   isExceptionSafe $ verifyTrackingNumber t
  -- describe "detectCarrier USPS" $ do
  --   it "detects the sample checksum in the docs" $
  --     detectCarrier "71123456789123456787" `shouldBe` Just USPS

  --   it "detects USPS certified" $
  --     detectCarrier "70000000000000000000" `shouldBe` Just USPS

  --   it "detects USPS certified with whitespace" $
  --     detectCarrier "7000 0000 0000 0000 0000" `shouldBe` Just USPS

  --   it "detects USPS delivery confirmation" $
  --     detectCarrier "9405501548007115480421" `shouldBe` Just USPS

  --   it "detects USPS delivery confirmation with spaces" $
  --     detectCarrier "9405 5015 4800 7115 4804 21" `shouldBe` Just USPS

  --   it "detects USPS tracking numbers beginning in 91" $
  --     detectCarrier "9105501548007115480421" `shouldBe` Just USPS

  --   it "detects USPS priority mail international" $
  --     detectCarrier "CP000000000US" `shouldBe` Just USPS

  --   it "detects USPS priority mail international with spaces" $
  --     detectCarrier "CP 000 000 000 US" `shouldBe` Just USPS

  -- describe "detectCarrier UPS" $
  --   it "detects 1Z tracking numbers" $
  --     detectCarrier "1Z17RV330222363530" `shouldBe` Just UPS

  -- describe "detectCarrier misses" $
  --   it "does not detect things that aren't USPS" $
  --     detectCarrier "wat" `shouldBe` Nothing

  -- describe "verifyUSPSCheck" $
  --   it "detects the sample checksum in the docs" $
  --     verifyUSPSCheck "71123456789123456787" `shouldBe` True
