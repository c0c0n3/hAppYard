{-# LANGUAGE UnicodeSyntax #-}
--
-- Encapsulates the running of an HXT transformation on a file.
--
module XProcessor (XTran(..), runTransformation) where

import Prelude.Unicode
import Control.Arrow.Unicode

import Control.Arrow
import Text.XML.HXT.Core
import Text.XML.HXT.Expat
import System.Exit


data XTran = XTran
    { inputDoc  ∷ FilePath    -- empty = read input doc from stdin
    , ouputDocs ∷ [FilePath]  -- empty = write everything to stdout
    , config    ∷ SysConfigList
    , transform ∷ IOSArrow XmlTree XmlTree
    }


-- produce an arrow to write the given tree to the associated filepath
writeA ∷ IOSArrow (FilePath, XmlTree) (IOSArrow ξ XmlTree)
writeA = arr (\(f, t) → root [] [constA t] ⋙ writeDocument [] f)
-- NB writeDocument calls getChildren, so we have to put back the HXT root.

process ∷ XTran → IOSArrow ξ XmlTree
process t = applyA $ configure ⋙ parse ⋙ transformDoc >>. bindFileName ⋙ writeA
    where
    configure    = configSysVars (config t)                    -- both read and write options
    parse        = readDocument [withExpat True] (inputDoc t)  -- always override expat option
    -- run transformation on actual doc root; discard HXT root and any other XML outside of
    -- the input doc root (processing instructions, comments, text)
    transformDoc = getChildren ⋙ (transform t `when` isElem)  
    -- bind each result tree to its corresponding file (use stdout after running out of
    -- file names)
    bindFileName = zip (ouputDocs t ++ repeat [])

--
-- Run an XML transformation.
-- * configure HXT read/write with (config t)
-- * parse (inputDoc t)
-- * pass its root node to (transform t)
-- * write each produced tree t₁, t₂, t₃, … to (ouputDocs t) = f₁, f₂, f₃…
--   (use stdout after running out of file names)
--
runTransformation ∷ XTran → IO Bool
runTransformation t = (runX $ process t ⋙ getErrStatus) >>= (return ∘ isSuccess)
    where
    isSuccess [errorCode] | errorCode >= c_err = False
    isSuccess _ = True

-- TODO Error Handling Not Working.  
-- Try run on a non-existent file: an exception is printed to stdout, but this
-- function still returns True!
