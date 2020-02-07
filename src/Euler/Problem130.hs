{-# LANGUAGE BangPatterns #-}

module Euler.Problem130
( problem130
) where

import Data.List.Ordered (minus,merge)
import Euler.Util
import Math.NumberTheory.Primes

a n = findDiv 1 1
    where findDiv !r !k
              | r == 0    = k
              | otherwise = findDiv ((10*r+1) `mod` n) (k+1)

problem130 = NoInput . show . sum $ take 25 [ n | n <- [3,5..] `minus` merge (map unPrime primes) [5,10..]
                                      , (n - 1) `mod` a n == 0 ]
