module Euler.Problem132
( problem132
) where

import Euler.Util
import GHC.Natural
import Math.NumberTheory.Primes
import Math.NumberTheory.Moduli

problem132 = let k = 10^9
             in NoInput . show . sum $ take 40 [ p | p <- primes
                                         , powSomeMod (10 `modulo` naturalFromInteger (9*p)) k == 1 ]
