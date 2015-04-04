module Euler.Problem204
( problem204
) where

import Euler.Util
import Math.NumberTheory.Primes
import Math.NumberTheory.Logarithms

generalizedHamming limit typ      = countHamming limit $ reverse ps
    where countHamming lim [p]    = 1 + length (map (p^) [1..integerLogBase p lim]) 
          countHamming lim (p:ps) = sum (map (flip countHamming ps . (lim `div`)) $ map (p^) [1..integerLogBase p lim]) + countHamming lim ps
          ps                      = takeWhile (<typ) primes

problem204 = NoInput . show $ generalizedHamming (10^9) 100
