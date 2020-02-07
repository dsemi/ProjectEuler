module Euler.Problem123
( problem123
) where

import Math.NumberTheory.Primes
import Math.NumberTheory.Moduli
import Euler.Util
import GHC.Natural

problem123 = NoInput . show . fst . head $ filter ((>10^10) . uncurry f) ps
    where pmi = powSomeMod
          f n p = case (pmi ((p+1) `modulo` naturalFromInteger (p^2)) n + pmi ((p-1) `modulo` naturalFromInteger (p^2)) n) of
                    SomeMod k -> getVal k
                    InfMod{} -> error "bad"
          border = ceiling . sqrt $ 10^10 + 2
          ps = dropWhile ((<border) . snd) $ zip [1..] $ map unPrime primes
