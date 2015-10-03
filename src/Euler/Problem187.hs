module Euler.Problem187
( problem187
) where

import Data.List (tails)
import Euler.Util
import Math.NumberTheory.Primes

problem187 = NoInput . show . length . concat . takeWhile (not . null) $ primePairs
    where mult xs = map (* head xs) xs
          primePairs = map (takeWhile (<10^8) . mult) $ tails primes
