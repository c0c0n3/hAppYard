{-# LANGUAGE UnicodeSyntax #-}
--
-- Utility functions to process XmlNode's directly, i.e. outside of XmlArrow.
--
module XmlNodeUtil where

import Prelude.Unicode

import Control.Arrow
import Data.AssocList
import Data.Maybe
import qualified Text.XML.HXT.DOM.XmlNode as XN


type LocalName = String


localPart ∷ XN.XmlNode ξ ⇒ ξ → LocalName
localPart = fromMaybe "" ∘ XN.getLocalPart

text ∷ XN.XmlNode ξ ⇒ ξ → String
text = fromMaybe "" ∘ XN.getText

attributes ∷ XN.XmlNode ξ ⇒ ξ → AssocList LocalName String
attributes = map (localPart &&& value) ∘ fromMaybe [] ∘ XN.getAttrl
    where
    value = headDef "" ∘ map text ∘ XN.getChildren
    headDef x [] = x         -- TODO use Safe package instead
    headDef x xs = head xs

attribute ∷ XN.XmlNode ξ ⇒ LocalName → ξ → String
attribute name = lookup1 name ∘ attributes

hasName ∷ XN.XmlNode ξ ⇒ LocalName → ξ → Bool
hasName name = (≡name) ∘ localPart
