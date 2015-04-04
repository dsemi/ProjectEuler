module Euler.Problem025
( problem25
) where

import Data.Function.Memoize
import Euler.Util

fibonacci = memoFix f
    where f :: (Integer -> Integer) -> Integer -> Integer
          f _ 1   = 1
          f _ 2   = 1
          f fib n = fib (n-1) + fib (n-2)

problem25 = NoInput . show $ head [x | x <- [1..], let f = fibonacci x, f `div` 10^999 >= 1]
