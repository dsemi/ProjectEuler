module Euler.Problem133
( problem133
) where

import Euler.Util
import Math.NumberTheory.Primes
import Math.NumberTheory.Moduli

problem133 = NoInput . show . sum $ [ p | p <- takeWhile (<100000) primes
                              , all (\x -> powerModInteger 10 x (9*p) /= 1) $ map (10^) [1..16] ]
