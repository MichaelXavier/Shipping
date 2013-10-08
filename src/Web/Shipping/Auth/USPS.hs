{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Web.Shipping.Auth.USPS ( Auth(..)
                              , HasAuth(..) ) where

import ClassyPrelude
import Control.Lens

data Auth = Auth { _userId :: Text } deriving (Show, Eq)

makeClassy ''Auth
