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
      let Right resp = parseXML happyResponse :: Either TrackResponseError TrackResponse
      resp ^. respTrackingNumber `shouldBe` TrackingNumber "1234"
      resp ^. respTrackSummary `shouldBe` TrackSummary "Delivered a while ago"
      resp ^. respTrackDetails `shouldBe` fromList [
                                            TrackDetail "8AM Oldest"
                                          , TrackDetail "9AM Older"
                                          , TrackDetail "10AM Most Recent"
                                          ]
    it "handles errors in the xml" $ do
      let Left err = parseXML errorResponse :: Either TrackResponseError TrackResponse
      err ^. respErrorNumber `shouldBe` Just "123"
      err ^. respErrorDescription `shouldBe` "Broke"

    it "handles unexpected problems generically" $ do
      let Left err = parseXML "<TrackResponse />" :: Either TrackResponseError TrackResponse
      err ^. respErrorNumber `shouldBe` Nothing
      err ^. respErrorDescription `shouldBe` "Missing TrackInfo"


parseXML :: FromXML f a => LText -> Either f a
parseXML txt = fromXML . documentRoot $ doc
  where Right doc = parseText def txt

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

errorResponse :: LText
errorResponse = [qc|
<TrackResponse>
  <Error>
    <Number>123</Number>
    <Source>WhoCares</Source>
    <Description>Broke</Description>
    <HelpFile>dunno</HelpFile>
    <HelpContext>dunno either</HelpContext>
  </Error>
</TrackResponse>
|]
