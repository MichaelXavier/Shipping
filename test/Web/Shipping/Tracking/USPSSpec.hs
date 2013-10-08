{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Web.Shipping.Tracking.USPSSpec (spec) where

import Prelude (head)
import ClassyPrelude
import Data.Default (def)
import Web.Shipping.Auth.USPS
import Web.Shipping.Tracking.USPS
import Web.Shipping.XML
import SpecHelper

spec :: Spec
spec = do
  describe "ToXML TrackRequest" $ do
    let tn   = TrackingNumber "1234"
    let auth = Auth "UID"
    let req  = TrackRequest tn auth 
    let e    = toXML req

    it "produces the correct xml" $ do
      e ^. name `shouldBe` "TrackRequest"
      e ^. attribute "USERID" `shouldBe` Just "UID"
      let trackingIds = e ^.. entire . el "TrackingID"
      length trackingIds `shouldBe` 1
      (head trackingIds) ^. attribute "ID" `shouldBe` Just "1234"

  describe "FromXML TrackResponse" $ do
    it "parses the happy path" $ do
      let Right resp = parseXML happyResponse :: Either Text TrackResponse
      resp ^. respTrackingNumber `shouldBe` TrackingNumber "1234"
      resp ^. respTrackSummary `shouldBe` TrackSummary "Delivered a while ago"
      resp ^. respTrackDetails `shouldBe` fromList [
                                            TrackDetail "8AM Oldest"
                                          , TrackDetail "9AM Older"
                                          , TrackDetail "10AM Most Recent"
                                          ]

parseXML :: FromXML a => LText -> Either Text a
parseXML txt = case parsed of
                 Right doc -> fromXML . documentRoot $ doc
                 Left e    -> Left . pack . show $ e
  where parsed = parseText def txt

happyResponse :: LText
happyResponse = [qc|
<TrackResponse>
  <TrackInfo ID="1234">
    <TrackSummary>Delivered a while ago</TrackSummary>
    <TrackDetail>10AM Most Recent</TrackDetail>
    <TrackDetail>9AM Older</TrackDetail>
    <TrackDetail>8AM Oldest</TrackDetail>
  </TrackInfo>
</TrackResponse>
|]
