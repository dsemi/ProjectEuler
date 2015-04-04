module Euler.Problem049
( problem49
) where

import Data.List
import Euler.Util
import Math.NumberTheory.Primes

p = map (permutations . show) $ filter isPrime [1001,1003..10000]

roughSequences = nub . map (sort . nub) $ map (map (\x -> read x :: Integer)) p

lessRoughSequences = filter ((>=3) . length) (map (filter conditions) roughSequences)
    where conditions x = isPrime x && x >= 1000

testableSequences = concatMap magic lessRoughSequences
    where magic xs = filter ((==3) . length) $ subsequences xs

arithmeticSequence [a,b,c] = let d = b-a in b+d == c

problem49 = NoInput . concatMap show . last . filter arithmeticSequence $ testableSequences
