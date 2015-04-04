module Euler.Problem058
( problem58
) where

import Euler.Util
import Math.NumberTheory.Primes

primeRatioOnDiagonals np n r
    | fromIntegral np / fromIntegral (n*2 - 1) < r = n
    | otherwise = primeRatioOnDiagonals (np + length [a | let n' = n^2+n+1, a <- [n',n'+n+1,n'+2*n+2], isPrime a]) (n+2) r


problem58 = NoInput . show $ primeRatioOnDiagonals 3 3 0.1

