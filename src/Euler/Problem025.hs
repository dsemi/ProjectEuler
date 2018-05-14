module Euler.Problem025
( problem25
) where

import Euler.Util

fibonacci = go 0 1
    where go :: Integer -> Integer -> Integer -> Integer
          go a b 0 = a
          go a b n = go b (a+b) (n-1)

problem25 = NoInput . show $ head [x | x <- [1..], let f = fibonacci x, f `div` 10^999 >= 1]
