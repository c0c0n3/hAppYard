{-# LANGUAGE UnicodeSyntax #-}
module ViewBox (removeDimensionUnits, addViewBox) where

import Prelude.Unicode
import Control.Arrow.Unicode

import Data.Char

import Text.XML.HXT.Core


addViewBox ∷ ArrowXml hom ⇒ hom XmlTree XmlTree
addViewBox = removeDimensionUnits ⋙ add ⋙ setDimensions
    where
    add = applyA (viewBoxValue ⋙ (arr $ addAttr "viewBox"))
    setDimensions =  processAttrl $ changeAttrValue (const "100%") `when` isDim

removeDimensionUnits ∷ ArrowXml hom ⇒ hom XmlTree XmlTree
removeDimensionUnits = processAttrl $ changeAttrValue takeDigits `when` isDim
    where
    takeDigits = takeWhile isDigit
 
isDim ∷ ArrowXml hom ⇒ hom XmlTree XmlTree
isDim = hasName "width" <+> hasName "height"

viewBoxValue ∷ ArrowXml hom ⇒ hom XmlTree String
viewBoxValue = (getAttrValue "width" &&& getAttrValue "height")
               ⋙ 
               (arr $ \(w, h) → "0 0 " ++ w ++ " " ++ h)
