--
-- Encapsulates the running of an HXT transformation on a file.
--
module XProcessor where

import Text.XML.HXT.Core
import Text.XML.HXT.Expat
import System.Exit


data XTran = XTran
    { inputDoc  :: FilePath
    , ouputDoc  :: FilePath
    , config    :: SysConfigList
    , transform :: IOSArrow XmlTree XmlTree
    }

process :: XTran -> IOSArrow b Int
process t = configSysVars (config t)
            >>>
            readDocument [withExpat True] (inputDoc t)
            >>>
            processChildren (processDocumentRootElement `when` isElem)
            >>>
            writeDocument [] (ouputDoc t)
            >>>
            getErrStatus
    where
    processDocumentRootElement = transform t

runTransformation :: XTran -> IO Bool
runTransformation t = (runX $ process t) >>= (return . isSuccess)
    where
    isSuccess [errorCode] | errorCode >= c_err = False
    isSuccess _ = True
