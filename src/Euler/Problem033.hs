module Euler.Problem033
( problem33
) where

import Data.Ratio
import Euler.Util

canceling :: Int -> Int -> Bool
canceling x y = d /= 0 && b == c && a % d == x % y
    where (a, b) = quotRem x 10
          (c, d) = quotRem y 10

problem33 = NoInput . show . denominator $ product [ a % b | a <- [10..99]
                                                   , b <- [a+1..99]
                                                   , canceling a b]
