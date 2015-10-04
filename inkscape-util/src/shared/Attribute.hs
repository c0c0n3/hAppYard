{-# LANGUAGE UnicodeSyntax #-}
{-
Functions to deal with attributes.
-}
module Attribute where

import Prelude.Unicode
import Control.Arrow.Unicode

import Control.Arrow
import Text.XML.HXT.Core



type LocalName = String
type Value = String


-- collect each attribute of the current node whose value satisfies the given predicate.
findByValue ∷ ArrowXml hom ⇒ (Value → Bool) → hom XmlTree XmlTree
findByValue p = getAttrl ⋙ lookup
    where
    lookup = (getChildren ⋙ getText ⋙ isA p) `guards` this

-- process the current node ξ so that it will contain an attribute with the specified 
-- value.
-- specifically:
-- ∃ attribute whose value ≡ v ⇒ return (a₁, ξ) (a₂, ξ) … 
--                                         a₁, a₂, … = matching attribute names
-- otherwise ⇒ return (n, ξ')
--                     where ξ' = ξ with added (n, v)
--
ensureValue ∷ ArrowXml hom ⇒ QName → Value → hom XmlTree (QName, XmlTree)
ensureValue n v = attrName &&& attrNode
    where
    exists   = findByValue (≡v)
    attrName = ifA exists (exists ⋙ getQName) (constA n)
    attrNode = ifA exists this (addQAttr n v)

-- add (or replace) an attribute
-- NB seems to be missing from HXT API? (see addAttr)
addQAttr ∷ ArrowXml hom ⇒ QName → Value → hom XmlTree XmlTree
addQAttr n v = addAttrl $ sqattr n v
