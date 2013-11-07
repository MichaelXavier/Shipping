{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.Reader
import Network.HTTPMock
import Web.Shipping.Tracking.USPS

-- FOR TESTING

main :: IO ()
main = print =<< trackDat


trackDat = runReaderT (getTrackingData myTrackingNumber) ctx

myTrackingNumber = TrackingNumber "bogus"

ctx = RequestContext realHTTPBackend myAuth

myAuth = Auth "098ZE57JN801"
