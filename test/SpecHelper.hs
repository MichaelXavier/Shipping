{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-imports #-}
module SpecHelper ( shouldBe
                  , it
                  , describe
                  , pending
                  , isExceptionSafe
                  , property
                  , qc
                  , ByteString
                  , Spec ) where
import ClassyPrelude
import Control.DeepSeq (NFData)
import Control.Spoon (spoon)
import Test.Hspec
import Test.QuickCheck ( property
                       , Arbitrary(..) )
import Text.InterpolatedString.Perl6 (qc)
import Web.Shipping.Types

isExceptionSafe :: NFData a => a -> Bool
isExceptionSafe = isJust . spoon

instance Arbitrary USPSTracking where
  arbitrary = USPSTracking . pack <$> arbitrary

instance Arbitrary UPSTracking where
  arbitrary = UPSTracking . pack <$> arbitrary
