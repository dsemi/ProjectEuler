module Euler.Problem023
( problem23
) where

import Data.Array
import Euler.Util
import Math.NumberTheory.Primes.Factorisation

n = 28124
abundant n = divisorSum n > 2*n
abundsArray = listArray (1,n) $ map abundant [1..n]
abunds = filter (abundsArray !) [1..n]

rests x = map (x-) $ takeWhile (<= x `div` 2) abunds
isSum = any (abundsArray !) . rests

problem23 = NoInput . show . sum . filter (not . isSum) $ [1..n]
