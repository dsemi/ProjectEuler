module Euler.Problem188
( problem188
) where

import Euler.Util
import Math.NumberTheory.ArithmeticFunctions
import Math.NumberTheory.Primes

problem188 = NoInput . show $ tetrate 1777 1855
    where modulus = 10^8
          phi     = totient (10^8)
          fastPower x 1 modulus = x `mod` modulus
          fastPower x n modulus
              | even n = fastPower ((x*x) `mod` modulus) (n `div` 2) modulus
              | otherwise = (x * fastPower x (n-1) modulus) `mod` modulus
          tetrate :: Int -> Int -> Int
          tetrate n 1 = n
          tetrate n k = fastPower n (tetrate n (k-1) `mod` phi) modulus
