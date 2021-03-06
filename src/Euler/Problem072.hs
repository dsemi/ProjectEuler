module Euler.Problem072
( problem72
) where

import Data.List (foldl1')
import Euler.Util
import Math.NumberTheory.Primes.Factorisation

mobius 1 = 1
mobius n
    | any ((>1) . snd) ps = 0
    | even (length ps)    = 1
    | otherwise           = -1
    where ps = factorise n

sumTotient n = (1 + foldl1' (+) [m*s | k <- [1..n], let m = mobius k, let s = (n `div` k)^2]) `div` 2

problem72 = NoInput . show $ sumTotient 1000000 - 1
