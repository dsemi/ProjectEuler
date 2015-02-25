module Euler.Problem080
( problem80
) where

import Data.List.Ordered (minus)
import Data.Char (digitToInt)
import Euler.Util

sumDigits n = sum . map digitToInt $ show n

findSqrt n limit = find (5*n,5)
    where find (a,b)
              | b >= 10^(limit+1) = b `div` 100
              | a >= b    = find (a-b, b+10)
              | otherwise = find (100*a, b*10 - 45)

problem80 = NoInputI . sum . map (sumDigits . flip findSqrt 100) $ [1..100] `minus` scanl1 (+) [1,3..]
