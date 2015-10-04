{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
--
-- Same transformation as the Yesod XML example: http://www.yesodweb.com/book/xml
-- Using XHT to see how it compares to xml-coduit.
--
module YesodExample (process) where

import Text.XML.HXT.Core


process :: ArrowXml a => a XmlTree XmlTree
process = deep doc

doc = isElm "document" >>> selem "html" [dead, body]

dead = selem "head" [
           selem "title" [ title ]]

title = (hasTitle `guards` getTitle) `orElse` (txt "Untitled Document")
    where
    getTitle = getAttrValue "title" >>> mkText
    hasTitle = getAttrl >>> hasName "title" >>> (neg isWhiteSpace)  -- could use hasAttr

body = selem "body" [ tags ]

tags = getChildren 
       >>> choiceA 
               [ isElm "para"   :-> tag "p"
               , isElm "em"     :-> tag "i"
               , isElm "strong" :-> tag "b"
               , isElm "image"  :-> tagWithAttrFilter "img" imgAttrl [] -- images can't have children
               , this           :-> this -- copy over as is if not one of the above
               ]

imgAttrl = renameHref `when` (hasName "href")
    where
    renameHref = changeAttrName $ mkName . const "src"

-- make a tag with the given name and copy over the element's attributes.
tag name = tagWithAttrFilter name this [ tags ]

-- make a tag with the given name and process the element's attributes using the given filter.
tagWithAttrFilter name f = mkelem name [ getAttrl >>> f ] 

isElm name = isElem >>> hasName name
