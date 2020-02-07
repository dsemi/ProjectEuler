module Euler.Problem357
( problem357
) where

import Data.Array.Unboxed
import Euler.Util
import Math.NumberTheory.Primes

divisorSumPrimes lim = filter (\x -> x == 1 || x `rem` 4 == 2 && isPGI x) $ map (subtract 1) ps
    where ps = takeWhile (< lim) $ map unPrime primes
          pArray :: UArray Integer Bool
          pArray = accumArray (||) False (1, lim) . zip ps $ repeat True
          isPGI n = all (pArray !) [ d + q | d <- [2 .. floor . sqrt $ fromIntegral n]
                                   , let (q,r) = quotRem n d
                                   , r == 0 ]

problem357 = NoInput . show $ sum . divisorSumPrimes $ 10^8
