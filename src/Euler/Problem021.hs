module Euler.Problem021
( problem21
) where

import Data.List (group)
import Math.NumberTheory.ArithmeticFunctions (sigma)
import Data.Map (fromList,keys,member,(!))
import Euler.Util

-- Gets the sum of the proper divisors of n
sumProperDivisors :: Int -> Int
sumProperDivisors n = sigma 1 n - n

-- Creates a map of all numbers from 1 to n to the sum of their respective proper divisors
-- Also filters out primes, perfect numbers, and numbers whose proper divisor sums are greater than n
divisorSumMaps n = fromList sums
    where sums = filter (\(a,b) -> b > 1 && b < n && a /= b) $ map (\x -> (x, sumProperDivisors x)) [1..n]

-- Folds over every element in the map, if a maps to another key and that maps back to a, then
--     add them to the running total
-- Does twice as many lookups as necessary, maybe fix later
sumAmicablePairs m = (`div` 2) . foldl amicable 0 $ keys m
    where amicable acc k = if member (m ! k) m && k == m ! (m ! k) then acc+k+(m ! k) else acc

problem21 = NoInput . show . sumAmicablePairs $ divisorSumMaps 9999
