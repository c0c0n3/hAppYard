{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Main where

import Text.XML.HXT.Core
import System.Environment
import System.Exit

import XProcessor
import YesodExample as Y


main :: IO ()
main = do
        [src, dst] <- getArgs
        isSuccess <- run src dst
        exitWith $ exitStatus isSuccess
    where
    exitStatus True  = ExitSuccess
    exitStatus False = ExitFailure (0-1)

run src dst = runTransformation xtran
    where
    xtran = XTran 
        { inputDoc  = src
        , ouputDoc  = dst
        , config    = [withValidate no]
        , transform = Y.process
        }

-- e.g. run "../yesod-example.xml" "../yesod-example.out.xml"        


{-
tree = constA "<r><x a='1' b='2'/></r>" >>> xread
attrs = changeAttrName $ mkName . attrMap . localPart
    where
    attrMap "a" = "a1"
    attrMap "b" = "b2"
    attrMap  x  =  x

test = runX (tree //> hasName "x" >>> processAttrl attrs)

prettyPrint t =  (runX . xshow $ t >>> indentDoc) >>= mapM_ putStrLn
-}