module Euler.Problem060
( problem60
) where

import Euler.Util

import Math.NumberTheory.Primes (primes, unPrime)
import Math.NumberTheory.Primes.Testing

concatsToPrime :: Int -> Int -> Bool
concatsToPrime m n = all (isPrime . fromIntegral) [cc m n 0 0, cc n m 0 0]
    where cc :: Int -> Int -> Int -> Int -> Int
          cc m 0 p c = c + m*10^p
          cc m n p c = cc m (n `div` 10) (p+1) (c + (n `mod` 10)*10^p)

minPrimePairSet :: Int -> [[Int]]
minPrimePairSet n = f 0 ps
    where ps :: [Int]
          ps = map fromIntegral . tail
               $ takeWhile (<10000) $ map unPrime primes
          pairs :: Int -> [Int]-> [Int]
          pairs n = filter (concatsToPrime n)
                    . dropWhile (<=n)
          f :: Int -> [Int] -> [[Int]]
          f c [] = if c == n then [[]] else []
          f c (p:prs) =
              if c == n then [[]]
              else concat [ map (p:) . f (c+1)
                            $ pairs p prs
                          , f c prs]

problem60 = NoInput . show $ sum . head $ minPrimePairSet 5
