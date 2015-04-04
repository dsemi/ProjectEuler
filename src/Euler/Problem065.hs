module Euler.Problem065
( problem65
) where

import Data.Ratio
import Data.Char (digitToInt)
import Data.List (intercalate)
import Euler.Util

x :: [Rational]
x = 1:(intercalate [1,1] $ map (\x -> [x]) [2,4..])

eIteration 1 = 2
eIteration n = (+2) . recip . foldl (\acc a -> a + recip acc) (x !! (n-2)) . reverse $ take (n-2) x

sumDigits n = sum . map digitToInt $ show n

problem65 = NoInput . show . sumDigits . numerator $ eIteration 100
