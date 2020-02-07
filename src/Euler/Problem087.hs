module Euler.Problem087
( problem87
) where

import Data.Array.Unboxed
import Euler.Util
import Math.NumberTheory.Primes

primePowerTriples :: Integer -> UArray Integer Bool
primePowerTriples n = accumArray (||) False (1, n)
                      [ (s, True) | x <- squares
                      , y <- takeWhile (<(n-x)) cubes
                      , z <- takeWhile (<(n-x-y)) tetras
                      , let s = x + y + z ]
    where squares = takeWhile (< n) $ map (^2) $ map unPrime primes
          cubes   = takeWhile (< n) $ map (^3) $ map unPrime primes
          tetras  = takeWhile (< n) $ map (^4) $ map unPrime primes

problem87 = let n = 50000000
            in NoInput . show . length . filter id . elems $ primePowerTriples n
