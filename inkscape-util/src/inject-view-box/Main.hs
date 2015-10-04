{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Prelude.Unicode

import Text.XML.HXT.Core

import Program
import XProcessor
import ScriptInjector


main :: IO ()
main = runAction $ run "" ""    -- i.e. src = stdin, dst = stdout

run src dst = runTransformation xtran
    where
    xtran = XTran 
        { inputDoc  = src
        , ouputDocs = [dst]
        , config    = [withValidate no]
        , transform = process
        }
