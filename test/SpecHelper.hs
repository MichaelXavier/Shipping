{-# LANGUAGE NoImplicitPrelude #-}
module SpecHelper ( shouldBe
                  , it
                  , describe
                  , isExceptionSafe
                  , property
                  , ByteString
                  , Spec ) where
import ClassyPrelude
import Control.DeepSeq (NFData)
import Control.Spoon (spoon)
import Data.Maybe (isJust)
import Test.Hspec
import Test.QuickCheck ( property
                       , Arbitrary(..) )

import Data.ByteString (ByteString(..))

import Web.Shipping.Types

isExceptionSafe :: NFData a => a -> Bool
isExceptionSafe = isJust . spoon

instance Arbitrary USPSTracking where
  arbitrary = USPSTracking . pack <$> arbitrary
