module Euler.Problem012
( problem12
) where

import Euler.Util
import Math.NumberTheory.Primes.Factorisation

problem12 = NoInputI $ head [x | x <- scanl1 (+) [1..], tau x > 500]
