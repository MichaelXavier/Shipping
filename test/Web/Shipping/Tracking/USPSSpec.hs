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
      e ^. name `shouldBe` "TrackFieldRequest"
      e ^. attribute "USERID" `shouldBe` Just "UID"
      let trackingIds = e ^.. entire . el "TrackingID"
      length trackingIds `shouldBe` 1
      head trackingIds ^. attribute "ID" `shouldBe` Just "1234"

  describe "FromXML TrackResponse" $ do
    it "parses the happy path" $ do
      let Right resp = parseXML happyResponse :: Either TrackResponseError TrackResponse
      resp ^. respTrackingNumber `shouldBe` TrackingNumber "1234"
      resp ^. respTrackSummary `shouldBe` TrackEvent "12:12pm" "May 21, 2001" "DELIVERED" "NEWTON" "IA" "50208" Nothing Nothing Nothing Nothing
      resp ^. respTrackDetails `shouldBe` fromList [
          TrackEvent "10:00 pm" "March 27, 2001" "ACCEPTANCE" "BLAINE" "WA" "98231" Nothing Nothing Nothing Nothing
        , TrackEvent "9:24 pm" "March 28, 2001" "ENROUTE" "DES MOINES" "IA" "50395" Nothing Nothing Nothing Nothing
                                          ]
    it "handles errors in the xml" $ do
      let Left err = parseXML errorResponse :: Either TrackResponseError TrackResponse
      err ^. respErrorNumber `shouldBe` Just "123"
      err ^. respErrorDescription `shouldBe` "Broke"

    it "handles unexpected problems generically" $ do
      let Left err = parseXML "<TrackResponse />" :: Either TrackResponseError TrackResponse
      err ^. respErrorNumber `shouldBe` Nothing
      err ^. respErrorDescription `shouldBe` "Missing element TrackInfo from TrackResponse"


parseXML :: FromXML f a => LText -> Either f a
parseXML txt = fromXML . documentRoot $ doc
  where Right doc = parseText def txt

happyResponse :: LText
happyResponse = [qc|
<TrackResponse>
  <TrackInfo ID="1234">
    <TrackSummary>
      <EventTime>12:12pm</EventTime>
      <EventDate>May 21, 2001</EventDate>
      <Event>DELIVERED</Event>
      <EventCity>NEWTON</EventCity>
      <EventState>IA</EventState>
      <EventZIPCode>50208</EventZIPCode>
      <EventCountry/>
      <FirmName></FirmName>
      <Name></Name>
      <AuthorizedAgent></AuthorizedAgent>
    </TrackSummary>
    <TrackDetail>
      <EventTime>9:24 pm</EventTime>
      <EventDate>March 28, 2001</EventDate>
      <Event>ENROUTE</Event>
      <EventCity>DES MOINES</EventCity>
      <EventState>IA</EventState>
      <EventZIPCode>50395</EventZIPCode>
      <EventCountry/>
      <FirmName/>
      <Name/>
      <AuthorizedAgent/>
    </TrackDetail>
    <TrackDetail>
      <EventTime>10:00 pm</EventTime>
      <EventDate>March 27, 2001</EventDate>
      <Event>ACCEPTANCE</Event>
      <EventCity>BLAINE</EventCity>
      <EventState>WA</EventState>
      <EventZIPCode>98231</EventZIPCode>
      <EventCountry/>
      <FirmName/>
      <Name/>
      <AuthorizedAgent/>
    </TrackDetail>
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
