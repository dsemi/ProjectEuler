module Euler.Problem047
( problem47
) where

import Data.List (nub,groupBy)
import Euler.Util
import Math.NumberTheory.Primes.Factorisation

problem47 = let numsWithNFactors = [ x | x <- [1..]
                                   , length (factorise x) == n]
            in NoInput . show $ findNConsec numsWithNFactors 0 0
    where n = 4
          findNConsec (num:nums) p c
              | c == n-1     = p + 1 - fromIntegral n
              | num - p == 1 = findNConsec nums num (c+1)
              | otherwise    = findNConsec nums num 0
