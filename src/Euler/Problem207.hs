module Euler.Problem207
( problem207
) where

import Data.Ratio
import Euler.Util

f n = floor (logBase 2 (fromIntegral n)) % n < 1%12345

roughSearch t
    | f t       = t `div` 2
    | otherwise = roughSearch (2*t)

fineSearch s e
    | mid == s  = e*e-e
    | f (mid-1) = fineSearch s mid
    | otherwise = fineSearch mid e
      where mid = s + (e-s) `div` 2

problem207 = let s = roughSearch 2
             in NoInputI . fineSearch s $ 2*s
