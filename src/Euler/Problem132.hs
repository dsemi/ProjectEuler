module Euler.Problem132
( problem132
) where

import Euler.Util
import Math.NumberTheory.Primes
import Math.NumberTheory.Moduli

problem132 = let k = 10^9
             in NoInput . show . sum $ take 40 [ p | p <- primes
                                         , powerModInteger 10 k (9*p) == 1 ]
