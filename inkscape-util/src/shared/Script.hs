{-# LANGUAGE UnicodeSyntax, Arrows, NoMonomorphismRestriction #-}
{-
Functions to embed/reference scripts in/from an SVG document.
-}
module Script (embed, reference, script) where

import Prelude.Unicode
import Control.Arrow.Unicode

import Text.XML.HXT.Core

import Attribute


{-
Links an external script, making sure the XLink namespace is properly referenced.
Specifically,

    reference "sum/script.js"

results in this being added to the svg children:

    <script xxx:href="sum/script.js"></script>

where 'xxx' is the local part of the XLink namespace declaration on the svg tag, 
if present.  If not present, then it is added with a local part of 'xlink':

    <svg … xmlns:xlink="http://www.w3.org/1999/xlink" … >

-}
reference ∷ ArrowXml hom ⇒ String → hom XmlTree XmlTree
reference href = linkA $< (svg ⋙ single ensureXLink)
    where
    ensureXLink = ensureValue (mkName "xmlns:xlink") "http://www.w3.org/1999/xlink"
    linkA (qname, doc) = constA doc ⋙ 
                         replaceChildren (getChildren <+> linkScript qname href)

-- embed given code in script tag as child of svg element.
embed ∷ ArrowXml hom ⇒ String → hom XmlTree XmlTree
embed code = svg ⋙ addScript
    where
    addScript = replaceChildren (getChildren <+> script code)
   
-- make script tag containing given code.
script ∷ ArrowXml hom ⇒ String → hom ξ XmlTree
script code = mkelem "script" [ sattr "type" "text/javascript" ] 
                              [ constA code ⋙ mkCdata ]

linkScript ∷ ArrowXml hom ⇒ QName → String → hom ξ XmlTree
linkScript xlink href = mkelem "script" [sattr src href] [txt ""]
    where
    src = localPart xlink ++ ":src"

svg ∷ ArrowXml hom ⇒ hom XmlTree XmlTree
svg = deep (isElem ⋙ hasName "svg")
