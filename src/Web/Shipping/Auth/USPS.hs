{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Web.Shipping.Auth.USPS ( USPSAuth(..)
                              , HasUSPSAuth(..) ) where

import ClassyPrelude
import Control.Lens

data USPSAuth = USPSAuth { _uspsUserId :: Text } deriving (Show, Eq)

makeClassy ''USPSAuth
