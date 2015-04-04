module Euler.Problem123
( problem123
) where

import Math.NumberTheory.Primes
import Math.NumberTheory.Moduli
import Euler.Util

problem123 = NoInput . show . fst . head $ filter ((>10^10) . uncurry f) ps
    where pmi = powerModInteger
          f n p = (pmi (p+1) n (p^2) + pmi (p-1) n (p^2)) `mod` (p^2)
          border = ceiling . sqrt $ 10^10 + 2
          ps = dropWhile ((<border) . snd) $ zip [1..] primes
