module Euler.Problem148
( problem148
) where

import Euler.Util
import Math.NumberTheory.Logarithms

tri n = n*(n+1) `div` 2

p148 ub p = findSol 0 ub (p^logp) (tp^logp)
    where logp = integerLogBase p ub
          tp   = tri p
          findSol sol 0 rows nz = sol
          findSol sol a rows nz = let (d,r) = a `divMod` rows
                                  in findSol (sol + nz * tri d) r (rows `div` p) (nz* (d+1) `div` tp)

problem148 = NoInputI $ p148 (10^9) 7
