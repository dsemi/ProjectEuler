module Euler.Problem043
( problem43
) where

import Data.List (nub,(\\))
import Euler.Util

threeDigitChars = [[a,b,c] | a <- ['0'..'9'], b <- ['0'..'9'], c <- ['0'..'9']]

multsS n = filter pandigital $ filter ((==0) . (`mod` n) . read) threeDigitChars

pandigital s = length s == length (nub s)

problem43 = NoInput . show . sum . map (read . addMissing)
            $ foldr (\x acc -> [ num | n <- multsS x
                               , b <- acc, tail n == take 2 b
                               , let num = n ++ drop 2 b
                               , pandigital num]) (multsS 17) [2,3,5,7,11,13]
    where addMissing xs = head (['0'..'9'] \\ xs):xs
