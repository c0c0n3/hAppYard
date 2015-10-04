{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module LayerSplitter (splitLayers) where

import Prelude.Unicode
import Control.Arrow.Unicode

import Data.List
import Data.Ord
import qualified Data.Text as T
import Text.XML.HXT.Core
import qualified Text.XML.HXT.DOM.XmlNode as XN

import Layer
import qualified XmlNodeUtil as XNU

{-
Remove all "lib" layers from the input document, then for each "regular" layer in the
input document, produce a copy of the input document in which all other layers have 
been removed; if no layer is found in the input, no output is produced.

We take a "regular" layer to be any "g" element below the root which is not labeled 
as "master" --i.e has an attribute whose local name is "label" and whose value equals 
"master", ignoring case and leading/trailing white-space.
A "lib" layer is any "g" element below the root which is not labeled with a name
starting with "lib-", ignoring case and leading white-space.

E.g. given this document as input

    <x:r xmlns:x="x-uri" xmlns="default">
        <g label="lib-1">this will be removed</g>
        <t a="a" b="b">
           some <b>text</b>
        </t>
        <g inkskape:label="master">will be ignored</g>
        <x:u a="a" b="b">
           this <g>will be ignored</g>
        </x:u>
        <g>first layer</g>
        <script>kaboom</script>
        <g x:label="lib-2">this will be removed</g>
        <g id="2"/>
    </x:r>

the following two documents would be produced

    <x:r xmlns:x="x-uri" xmlns="default">
        
        <t a="a" b="b">
           some <b>text</b>
        </t>
        <g inkskape:label="master">will be ignored</g>
        <x:u a="a" b="b">
           this <g>will be ignored</g>
        </x:u>
        <g>first layer</g>
        <script>kaboom</script>
        
        
    </x:r>

    <x:r xmlns:x="x-uri" xmlns="default">
        
        <t a="a" b="b">
           some <b>text</b>
        </t>
        <g inkskape:label="master">will be ignored</g>
        <x:u a="a" b="b">
           this <g>will be ignored</g>
        </x:u>
        
        <script>kaboom</script>
        
        <g id="2"/>
    </x:r>

-}
splitLayers ∷ ArrowXml hom ⇒ hom XmlTree XmlTree
splitLayers = applyA $ splitChildren >>. map setChildren

splitChildren ∷ ArrowXml hom ⇒ hom XmlTree XmlTrees
splitChildren = (getChildren ⋙ removeLibLayers) >>. splitOnMatch isLayer

-- For each x ∈ xs s.t. p x (xs = input list; p = input predicate), extract the 
-- sub-sequence ys of xs s.t. y ∈ ys ⇒ (y ≡ x) ∨ (not p y).  
-- List all such sub-sequences.
-- E.g. 
--     f (`elem` "+=") "u+x=z"  ~~>  ["u+xz","ux=z"]
--     f (`elem` "AB") "u+x=z"  ~~>  []   
--
splitOnMatch ∷ (ξ → Bool) → [ξ] → [[ξ]]
splitOnMatch p = map ψ ∘ φ ∘ partition (p ∘ snd) ∘ zip [1..]
    where
    φ (x,y) = map (:y) x
    ψ = map snd ∘ sortBy (comparing fst)

