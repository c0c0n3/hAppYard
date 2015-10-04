{-# LANGUAGE UnicodeSyntax, Arrows, QuasiQuotes #-}
module ScriptInjector (process) where

import Prelude.Unicode
import Control.Arrow.Unicode

import Text.Heredoc
import Text.XML.HXT.Core

import Script
import ViewBox


process ∷ ArrowXml a ⇒ a XmlTree XmlTree
process = embed code ⋙ removeDimensionUnits

code = [there|src/inject-view-box/toggleViewBox.js|]

