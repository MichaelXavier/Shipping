module Web.Shipping.Tracking.Numbers (TrackingNumber(..)) where

import Data.ByteString (ByteString)

data TrackingNumber = USPSTracking ByteString |
                      FedExTracking ByteString |
                      UPSTracking ByteString
                      deriving (Show, Eq)
