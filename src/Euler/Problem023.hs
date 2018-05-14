module Euler.Problem023
( problem23
) where

import Data.Array
import Euler.Util
import Math.NumberTheory.ArithmeticFunctions

n = 28124
abundant :: Int -> Bool
abundant n = sigma 1 n > 2*n
abundsArray = listArray (1,n) $ map abundant [1..n]
abunds = filter (abundsArray !) [1..n]

rests x = map (x-) $ takeWhile (<= x `div` 2) abunds
isSum = any (abundsArray !) . rests

problem23 = NoInput . show . sum . filter (not . isSum) $ [1..n]
