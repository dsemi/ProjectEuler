module Euler.Problem006
( problem6
) where

import Euler.Util

sumSquareDifference :: Int -> Int
sumSquareDifference n = (n*(n+1) `div` 2)^2 - sum (map (^2) [1..n])

problem6 = NoInput . show $ sumSquareDifference 100
