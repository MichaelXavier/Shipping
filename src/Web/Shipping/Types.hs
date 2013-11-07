{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
module Web.Shipping.Types ( USPSTracking(..)
                          , UPSTracking(..)
                          , FedExTracking(..)
                          , HasTrackingNumber(..) ) where

import ClassyPrelude
import Control.Lens 

-- These are now invalid
newtype USPSTracking   = USPSTracking  { _uspsTrackingNumber :: Text } deriving (Show, Eq)
newtype UPSTracking    = UPSTracking   { _upsTrackingNumber :: Text } deriving (Show, Eq)
newtype FedExTracking  = FedExTracking { _fedeXTrackingNumber :: Text } deriving (Show, Eq)

makeFields ''USPSTracking
makeFields ''UPSTracking
makeFields ''FedExTracking
