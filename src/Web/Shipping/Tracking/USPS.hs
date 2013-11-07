{-# LANGUAGE FlexibleContexts #-}
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
                                  , TrackEvent(..)
                                  , HasTrackEvent(..)
                                  , getTrackingData
                                  , RequestContext(..)
                                  , module Web.Shipping.Auth.USPS
                                  ) where

import ClassyPrelude hiding (Element)
import Control.Lens
import Control.Monad.Reader hiding (mapM)
import qualified Data.Map as M
import Network.Http.Client ( inputStreamBody )
import System.IO.Streams.ByteString ( fromLazyByteString )
import qualified System.IO.Streams as S
import Network.HTTPMock.Types
import Web.Shipping.XML
import Web.Shipping.Auth.USPS

newtype TrackingNumber = TrackingNumber { _trackingText :: Text } deriving (Show, Eq)

makeClassy ''TrackingNumber

data TrackRequest = TrackRequest { _reqTrackingNumber :: TrackingNumber
                                 , _reqAuth           :: Auth } deriving (Show, Eq)

makeClassy ''TrackRequest

-- is the a necessary?
data RequestContext a = RequestContext {
    ctxBackend :: HTTPBackend IO a
  , ctxAuth    :: Auth
  }

reqUIDText :: Lens' TrackRequest Text
reqUIDText = reqAuth . userId

reqTrackingText :: Lens' TrackRequest Text
reqTrackingText = reqTrackingNumber . trackingText

instance ToXML TrackRequest where
  toXML r = elm & name .~ "TrackFieldRequest"
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

data TrackEvent = TrackEvent { _eventTime            :: Text -- TODO; time
                             , _eventDate            :: Text --TODO: combine with time?
                             , _eventType            :: Text
                             , _eventCity            :: Text
                             , _eventState           :: Text
                             , _eventZIPCode         :: Text
                             , _eventZIPCountry      :: Maybe Text
                             , _eventFirmName        :: Maybe Text
                             , _eventName            :: Maybe Text
                             , _eventAuthorizedAgent :: Maybe Bool
                             } deriving (Show, Eq)

makeClassy ''TrackEvent

data TrackResponse = TrackResponse { _respTrackingNumber :: TrackingNumber
                                   , _respTrackSummary   :: TrackEvent
                                   , _respTrackDetails   :: Seq TrackEvent } deriving (Show, Eq)

makeClassy ''TrackResponse

instance FromXML TrackResponseError TrackResponse where
  fromXML e = either (const parseSuccess) Left parseError
    where parseSuccess    = either synthesizeError Right $ fromXML e
          parseError      = fromXML e :: Either Text TrackResponseError
          synthesizeError = Left . TrackResponseError Nothing

instance FromXML Text TrackEvent where
  fromXML e = do
    time        <- e `elText'` "EventTime"
    date        <- e `elText'` "EventDate"
    et          <- e `elText'` "Event"
    city        <- e `elText'` "EventCity"
    state       <- e `elText'` "EventState"
    zip         <- e `elText'` "EventZIPCode"
    let country = e ^? elText "EventCountry"
    let firm    = e ^? elText "EventFirmName"
    let name    = e ^? elText "Name"
    let aa      = e ^? elText "AuthorizedAgent" ^. to (fmap (=="true"))
    return $ TrackEvent time date et city state zip country firm name aa

instance FromXML Text TrackResponseError where
  fromXML e = do
    r     <- e  `elLookup'` "Error"
    let c = r ^? elText "Number"
    d     <- r  `elText'` "Description"
    return $ TrackResponseError c d
 
instance FromXML Text TrackResponse where
  fromXML e = do
    r  <- e  `elLookup'` "TrackResponse"
    ti <- r  `elLookup'` "TrackInfo"
    tn <- ti `attribute'` "ID"
    summary       <- fromXML =<< e  `elLookup'` "TrackSummary"
    --TODO: cleanup
    rChronDetails <- mapM fromXML $ e ^.. elLookup "TrackDetail"

    return $ TrackResponse (TrackingNumber tn) summary (buildHistory rChronDetails)
    where buildHistory = fromList . reverse


--TODO: handle exceptions into TrackResponseError
--TODO: reader on auth
getTrackingData :: ( MonadReader (RequestContext (Either TrackResponseError TrackResponse)) m
                   , Functor m
                   , MonadIO m
                   , Applicative m)
                   => TrackingNumber
                   -> m (Either TrackResponseError TrackResponse)
getTrackingData num = do
  tReq       <- TrackRequest <$> pure num <*> asks ctxAuth
  reqBackend <- asks ctxBackend
  liftIO $ xmlRequest responseException reqBackend tReq

--TODO: extract to XML or something
xmlRequest :: (ToXML req, FromXML err resp) => (SomeException -> err) -> HTTPBackend IO (Either err resp) -> req -> IO (Either err resp)
xmlRequest errHandler (HTTPBackend reqBackend) req = reqBackend buildRequest generateBody parseBody
  where buildRequest = Request POST "production.shippingapis.com" 80 "/ShippingAPITest.dll" []
        generateBody outStream = do inStream <- reqBodyStream
                                    inputStreamBody inStream outStream
        reqBodyStream = fromLazyByteString . renderAsDocRoot $ req
        --TODO: handle non-200 resp
        parseBody _resp inStream = parseFromXML errHandler . fromChunks <$> S.toList inStream

responseException :: SomeException -> TrackResponseError
responseException e = TrackResponseError Nothing (pack . show $ e)

---- Helpers

elText :: Name -> Traversal' Element Text
elText n = elLookup n . text

elText' :: Element -> Name -> Either Text Text
elText' el n = el ^? elText n ^. to (m2e $ "Missing " <> n ^. _nameLocalName)

attribute' :: Element -> Name -> Either Text Text
attribute' el n = el ^. attribute n ^. to (m2e $ mconcat [ "Missing attribute "
                                                         , n ^. _nameLocalName
                                                         , " from "
                                                         , el ^. localName ])

elLookup :: Name -> Traversal' Element Element
elLookup n = entire . el n

elLookup' :: Element -> Name -> Either Text Element
elLookup' el n = el ^? elLookup n ^. to (m2e $ mconcat [ "Missing element " 
                                                       , n ^. _nameLocalName 
                                                       , " from "
                                                       , el ^. localName ])

m2e :: b -> Maybe a -> Either b a
m2e b = maybe (Left b) Right
