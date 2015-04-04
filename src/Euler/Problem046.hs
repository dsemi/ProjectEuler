module Euler.Problem046
( problem46
) where

import Data.List.Ordered
import Math.NumberTheory.Primes
import Euler.Util

hasGoldbach n = any isPrime . takeWhile (>0) $ map ((n-) . (*2) . (^2)) [1..]

problem46 = NoInput . show . head . filter (not . hasGoldbach) $ [3,5..] `minus` primes
