{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Web.Shipping.Tracking.USPS ( TrackingNumber(..)
                                  , HasTrackingNumber(..)
                                  , TrackRequest(..)
                                  , HasTrackRequest(..)
                                  , TrackSummary(..)
                                  , HasTrackSummary(..)
                                  , TrackDetail(..)
                                  , HasTrackDetail(..)
                                  , TrackResponse(..)
                                  , HasTrackResponse(..)
                                  , TrackResponseError(..)
                                  , HasTrackResponseError(..)
                                  ) where

import ClassyPrelude hiding (Element)
import Control.Lens
import Data.EitherR
import qualified Data.Map as M
import Web.Shipping.XML
import Web.Shipping.Auth.USPS

newtype TrackingNumber = TrackingNumber { _trackingText :: Text } deriving (Show, Eq)

makeClassy ''TrackingNumber

data TrackRequest = TrackRequest { _reqTrackingNumber :: TrackingNumber
                                 , _reqAuth           :: Auth } deriving (Show, Eq)

makeClassy ''TrackRequest

reqUIDText :: Lens' TrackRequest Text
reqUIDText = reqAuth . userId

reqTrackingText :: Lens' TrackRequest Text
reqTrackingText = reqTrackingNumber . trackingText

instance ToXML TrackRequest where
  toXML r = elm & name .~ "TrackRequest"
                & attrs .~ M.singleton "USERID" (r ^. reqUIDText)
                & nodes .~ [
                  eln & _Element . name .~ "TrackingID"
                      & _Element . attrs .~ M.singleton "ID" (r ^. reqTrackingText)
                ]

newtype TrackSummary = TrackSummary { _summaryText :: Text } deriving (Show, Eq)

makeClassy ''TrackSummary

newtype TrackDetail = TrackDetail { _detailText :: Text } deriving (Show, Eq)

makeClassy ''TrackDetail

data TrackResponseError =  TrackResponseError { _respErrorNumber      :: Maybe Text
                                              , _respErrorDescription :: Text } deriving (Show, Eq)

makeClassy ''TrackResponseError

data TrackResponse = TrackResponse { _respTrackingNumber :: TrackingNumber
                                   , _respTrackSummary   :: TrackSummary
                                   , _respTrackDetails   :: Seq TrackDetail } deriving (Show, Eq)

makeClassy ''TrackResponse

instance FromXML TrackResponseError TrackResponse where
  fromXML e = either (const parseSuccess) Left parseError
    where parseSuccess    = either synthesizeError Right $ fromXML e
          parseError      = fromXML e :: Either Text TrackResponseError
          synthesizeError = Left . TrackResponseError Nothing

instance FromXML Text TrackResponseError where
  fromXML e = do
    r <- e  ^? elLookup "Error"     ^. to (m2e "Missing Error")
    let c = r ^? elText "Number"
    d <- r  ^? elText "Description" ^. to (m2e "Missing Description")
    return $ TrackResponseError c d
 
instance FromXML Text TrackResponse where
  fromXML e = do
    r  <- e  ^? elLookup "TrackResponse" ^. to (m2e "Missing TrackResponse")
    ti <- r  ^? elLookup "TrackInfo"     ^. to (m2e "Missing TrackInfo")
    tn <- ti ^. attribute "ID"           ^. to (m2e "Missing ID")
    summary  <- e  ^? elText "TrackSummary"    ^. to (m2e "Missing TrackSummary")
    let rChron = e  ^.. elLookup "TrackDetail" . text
    return $ TrackResponse (TrackingNumber tn) (TrackSummary summary) (buildHistory rChron)
    where buildHistory = fromList . reverse . map TrackDetail

elText :: Name -> Traversal' Element Text
elText n = elLookup n . text

elLookup :: Name -> Traversal' Element Element
elLookup n = entire . el n

m2e :: b -> Maybe a -> Either b a
m2e b = maybe (Left b) Right
