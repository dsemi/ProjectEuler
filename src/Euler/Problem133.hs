{-# LANGUAGE DataKinds #-}

module Euler.Problem133
( problem133
) where

import Euler.Util
import GHC.Natural
import Math.NumberTheory.Primes
import Math.NumberTheory.Moduli

problem133 = NoInput . show . sum $ [ p | p <- takeWhile (<100000) $ map unPrime primes
                              , all (\x -> powSomeMod (10 `modulo` naturalFromInteger (9*p)) x /= 1) $ map (10^) [1..16] ]
