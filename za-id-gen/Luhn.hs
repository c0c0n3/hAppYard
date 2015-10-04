--
-- see:
-- * http://en.wikipedia.org/wiki/Luhn_algorithm
-- * http://rosettacode.org/wiki/Luhn_test_of_credit_card_numbers#Haskell
--
module Luhn 
        (checks, addCheckDigit) 
where

import Control.Arrow
import Data.Char
import Data.List


type FullNumber = String
type PartialNumber = String

checksum :: [Int] -> Int
checksum = sum . map sumDigits . zipWith (*) (cycle [1, 2]) . reverse
    where
    sumDigits = uncurry (+) . (`quotRem` 10)  -- NB assumes input <= 20, i.e. at most 2 digits

checkDigit :: [Int] -> Int
checkDigit = (`mod` 10) . (* 9) . checksum
    
checks :: FullNumber -> Bool
checks =  (0 ==) . (`mod` 10) . checksum . map digitToInt

addCheckDigit :: PartialNumber -> FullNumber
addCheckDigit = uncurry (++) . (id &&& computeCheckDigit)
    where
    computeCheckDigit = (:[]) . intToDigit . checkDigit . map digitToInt . (++ "0")
