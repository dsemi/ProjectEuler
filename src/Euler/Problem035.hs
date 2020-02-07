module Euler.Problem035
( problem35
) where

import Data.List (foldl',foldl1')
import Math.NumberTheory.Primes (primes, unPrime)
import Math.NumberTheory.Primes.Testing
import Euler.Util

rotations n = foldl' rotate [n] [1..numRotations]
    where numRotations = length (show n) -1
          rotate nums@(num:_) _ = let d = num `mod` 10
                                  in d*10^numRotations + num `div` 10:nums

problem35 = NoInput . show $ foldl1' (+) [ 1 | x <- takeWhile (<1000000) $ map unPrime primes
                                   , all isPrime $ rotations x]
