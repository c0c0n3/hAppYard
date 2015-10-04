{-# LANGUAGE UnicodeSyntax, QuasiQuotes #-}
module Prezi (makePresentation) where

import Prelude.Unicode
import Control.Arrow.Unicode

import Text.Heredoc
import Text.XML.HXT.Core

import Layer
import Script
import ViewBox


makePresentation ∷ ArrowXml hom ⇒ hom XmlTree XmlTree
makePresentation = addViewBox ⋙ transformChildren
    where
    transformChildren = replaceChildren $ processLayers <+> scripts
    processLayers     = getChildren ⋙ removeLibLayers ⋙ markLayers
    markLayers        = addAttr "class" "layer" `when` isA isLayer
    scripts           = script d3Code <+> script slideShowCode <+> script slideShowRunnerCode

d3Code = [there|src/presentation/d3.v3.min.js|]
slideShowCode = [there|src/presentation/SlideShow.js|]
slideShowRunnerCode = [there|src/presentation/SlideShowRunner.js|]
