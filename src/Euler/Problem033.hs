module Euler.Problem033
( problem33
) where

import Data.Ratio
import Euler.Util

canceling :: (Integral a) => a -> a -> Bool
canceling n d
    | n `mod` 10 /= d `div` 10 || n == d || nd == 0 = False
    | otherwise = n%d == nn%nd
    where 
      nn = n `div` 10
      nd = d `mod` 10

problem33 = NoInputI . denominator $ product [ a%b | a <- [10..99]
                                             , b <- [a+1..99]
                                             , canceling a b]
