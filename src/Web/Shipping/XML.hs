{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Shipping.XML ( FromXML(..)
                        , ToXML(..)
                        , module Text.XML.Lens
                        , tNode
                        , elm
                        , eln
                        ) where

import ClassyPrelude hiding (Element)
import Control.Lens
import Text.XML.Lens

class FromXML t where
  fromXML :: Element -> Either Text t

class ToXML t where
  toXML :: t -> Element

tNode :: Name -> Text -> Node
tNode n t = eln & _Element . name .~ n
                & _Element . nodes .~ [NodeContent t]

elm :: Element
elm = Element "" mempty mempty

eln :: Node
eln = NodeElement elm
