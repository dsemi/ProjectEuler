module Euler.Problem006
( problem6
) where

import Euler.Util

problem6 = let n = 100
           in NoInputI $ (n*(n+1) `div` 2)^2 - sum (take n (scanl1 (+) [1,3..]))
