module Euler.Problem060
( problem60
) where

import Control.Monad
import Data.Function.Memoize
import Data.List.Ordered
import Data.Maybe
import Euler.Util
import Math.NumberTheory.Primes

concatsToPrime :: Int -> Int -> Bool
concatsToPrime m n = all (isPrime . fromIntegral) [cc m n 0 0, cc n m 0 0]
    where cc :: Int -> Int -> Int -> Int -> Int 
          cc m 0 p c = c + m*10^p
          cc m n p c = cc m (n `div` 10) (p+1) (c + (n `mod` 10)*10^p)

ps :: [Int]
ps = map fromIntegral . tail $ takeWhile (<10000) primes

pairs :: Int -> [Int]
pairs = memoize findPrimeConcats
    where findPrimeConcats n = filter (concatsToPrime n) $ dropWhile (<=n) ps

minPrimePairSet :: Int -> Int
minPrimePairSet n = fromJust $ pps 0 ps 0
    where pps :: Int -> [Int] -> Int -> Maybe Int
          pps c prs t
              | c == n = Just t
              | otherwise = listToMaybe $ mapMaybe (\p -> pps (c+1) (prs `isect` pairs p) (t+p)) prs 


problem60 = NoInputI $ minPrimePairSet 5



