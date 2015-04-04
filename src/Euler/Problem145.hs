module Euler.Problem145
( problem145
) where

import Euler.Util

numSolutions n
    | even n         = let k = n `div` 2
                       in 20 * 30^(k - 1)
    | n `mod` 4 == 3 = let k = (n - 3) `div` 4
                       in 5 * 20^(k+1) * 25^k
    | otherwise      = 0

problem145 = NoInput . show . sum $ map numSolutions [1..9]
