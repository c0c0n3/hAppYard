{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Prelude.Unicode

import Text.XML.HXT.Core
import System.Environment
import System.FilePath

import Program
import XProcessor
import LayerSplitter


main = do
    [inputPath] ← getArgs
    runAction $ run inputPath

run inputPath = runTransformation xtran
    where
    xtran = XTran
        { inputDoc  = inputPath
        , ouputDocs = generateOutputPaths inputPath
        , config    = [withValidate no]
        , transform = splitLayers
        }

generateOutputPaths = makePaths ∘ splitExtension 
    where
    makePath (name, ext) k = name ++ "." ++ show k ++ ext  
    makePaths (name, ext)  = map (makePath (name, ext)) [1..]
