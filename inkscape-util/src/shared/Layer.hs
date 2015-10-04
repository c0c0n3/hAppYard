{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
{-
Functions to deal with layers.

We take a "regular" layer to be any "g" element which is not labeled as "master"--i.e has
an attribute whose local name is "label" and whose value equals "master", ignoring case 
and leading/trailing white-space.
A "lib" layer is any "g" element which is not labeled with a name starting with "lib-", 
ignoring case and leading white-space.
-}
module Layer (isLayer, removeLibLayers) where

import Prelude.Unicode
import Control.Arrow.Unicode

import qualified Data.Text as T
import Text.XML.HXT.Core
import qualified Text.XML.HXT.DOM.XmlNode as XN

import qualified XmlNodeUtil as XNU


-- filter out lib layers.
removeLibLayers ∷ ArrowXml hom ⇒ hom XmlTree XmlTree
removeLibLayers = filterA (neg isLibLayer)

-- test: g element with no label set to "master" 
-- (ignoring case and leading/trailing white-space).
isLayer ∷ XN.XmlNode ξ ⇒ ξ → Bool
isLayer n = (XNU.hasName "g") n ∧ (not ∘ isMaster) n
    where
    isMaster n = labelValue n ≡ "master"

-- test: g element with label starting with "lib-"
-- (ignoring case and leading white-space).
isLibLayer ∷ ArrowXml hom ⇒ hom XmlTree XmlTree
isLibLayer = isElem ⋙ hasName "g" ⋙ isA lib
    where
    lib = T.isPrefixOf "lib-" ∘ labelValue

labelValue ∷ XN.XmlNode ξ ⇒ ξ → T.Text
labelValue = T.toLower ∘ T.strip ∘ T.pack ∘ XNU.attribute "label" 

