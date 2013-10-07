{-# LANGUAGE OverloadedStrings #-}
module Web.Shipping.Tracking.USPSSpec (spec) where

import Web.Shipping.Auth.USPS
import Web.Shipping.Tracking.USPS
import Web.Shipping.XML
import SpecHelper

spec :: Spec
spec = do
  describe "ToXML USPSTrackRequest" $ do
    let tn   = USPSTrackingNumber "1234"
    let auth = USPSAuth "UID"
    let req  = USPSTrackRequest tn auth 
    let e    = toXML req

    it "produces the correct xml" $ do
      e ^. name `shouldBe` "TrackRequest"
      e ^. attribute "USERID" `shouldBe` Just "UID"
      let trackingIds = e ^.. entire . el "TrackingID"
      length trackingIds `shouldBe` 1
      (head trackingIds) ^. attribute "ID" `shouldBe` Just "1234"
