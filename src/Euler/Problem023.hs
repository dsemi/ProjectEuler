module Euler.Problem023
( problem23
) where

import Data.Array
import Euler.Util
import Math.NumberTheory.Primes.Factorisation

n = 28124
abundant n = divisorSum n > 2*n
abunds_array = listArray (1,n) $ map abundant [1..n]
abunds = filter (abunds_array !) [1..n]

rests x = map (x-) $ takeWhile (<= x `div` 2) abunds
isSum = any (abunds_array !) . rests
 
problem23 = NoInputI . sum . filter (not . isSum) $ [1..n]
