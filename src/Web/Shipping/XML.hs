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
                        , renderAsDocRoot
                        , parseFromXML
                        ) where

import ClassyPrelude hiding (Element)
import Control.Lens
import Control.Monad ( (<=<) )
import Data.Default
import Data.EitherR ( fmapL )
import Text.XML ( parseText -- needed?
                , parseLBS
                , renderLBS
                , Prologue(..)
                )
import Text.XML.Lens

class FromXML f t where
  fromXML :: Element -> Either f t

class ToXML t where
  toXML :: t -> Element

tNode :: Name -> Text -> Node
tNode n t = eln & _Element . name .~ n
                & _Element . nodes .~ [NodeContent t]

renderAsDocRoot :: ToXML a => a -> LByteString
renderAsDocRoot e = renderLBS def doc
  where doc = Document (Prologue [] Nothing []) (toXML e) []

parseFromXML :: FromXML f t => (SomeException -> f) -> LByteString -> Either f t
parseFromXML errorHandler = fromXML . documentRoot <=< fmapL errorHandler . parseLBS def

elm :: Element
elm = Element "" mempty mempty

eln :: Node
eln = NodeElement elm
