{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Web.Shipping.Tracking.USPS ( USPSTrackingNumber(..)
                                  , HasUSPSTrackingNumber(..)
                                  , USPSTrackRequest(..)
                                  , HasUSPSTrackRequest(..) ) where

import ClassyPrelude
import Control.Lens
import qualified Data.Map as M
import Web.Shipping.XML
import Web.Shipping.Auth.USPS

newtype USPSTrackingNumber = USPSTrackingNumber { _uspsTrackingText :: Text } deriving (Show, Eq)

makeClassy ''USPSTrackingNumber

data USPSTrackRequest = USPSTrackRequest { _uspsReqTrackingNumber :: USPSTrackingNumber
                                         , _uspsReqAuth           :: USPSAuth } deriving (Show, Eq)

makeClassy ''USPSTrackRequest

reqUIDText :: Lens' USPSTrackRequest Text
reqUIDText = uspsReqAuth . uspsUserId

reqTrackingText :: Lens' USPSTrackRequest Text
reqTrackingText = uspsReqTrackingNumber . uspsTrackingText

instance ToXML USPSTrackRequest where
  toXML r = elm & name .~ "TrackRequest"
                & attrs .~ M.singleton "USERID" (r ^. reqUIDText)
                & nodes .~ [
                  eln & _Element . name .~ "TrackingID"
                      & _Element . attrs .~ M.singleton "ID" (r ^. reqTrackingText)
                ]
