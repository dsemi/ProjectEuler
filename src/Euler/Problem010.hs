module Euler.Problem010
( problem10
) where

import Math.NumberTheory.Primes
import Euler.Util

problem10 = NoInput . show . sum $ takeWhile (<2*10^6) $ map unPrime primes
