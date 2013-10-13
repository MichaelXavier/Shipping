{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Shipping.XML ( FromXML(..)
                        , ToXML(..)
                        , parseText
                        , module Text.XML.Lens
                        , tNode
                        , elm
                        , eln
                        ) where

import ClassyPrelude hiding (Element)
import Control.Lens
import Text.XML (parseText)
import Text.XML.Lens

class FromXML f t where
  fromXML :: Element -> Either f t

class ToXML t where
  toXML :: t -> Element

tNode :: Name -> Text -> Node
tNode n t = eln & _Element . name .~ n
                & _Element . nodes .~ [NodeContent t]

elm :: Element
elm = Element "" mempty mempty

eln :: Node
eln = NodeElement elm
