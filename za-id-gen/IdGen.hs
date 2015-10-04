--
-- see:
-- * http://freelance-it-consultant.com/blog/generate-valid-south-african-id-number-javascript-and-add-it-extension-selenium
--
module IdGen where

import Data.Char
import Luhn

type DoB = String
data Gender = Male | Female
type Random3 = String
type IdNumber = String
type LessThan1000 = Int

genderToDigit Male = "6"
genderToDigit Female = "4"

generateId :: DoB -> Gender -> Random3 -> IdNumber
generateId yymmdd gender random = addCheckDigit . concat $
                                    [ yymmdd 
                                    , genderToDigit gender
                                    , random
                                    , "00"
                                    ]

generateIds :: DoB -> Gender -> LessThan1000 -> [IdNumber]
generateIds yymmdd gender upTo = map (generateId yymmdd gender) . map pad $ [0..upTo]
    where
    pad x = reverse . take 3 $ (reverse . show $ x) ++ repeat '0'
