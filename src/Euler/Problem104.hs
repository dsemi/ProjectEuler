module Euler.Problem104
( problem104
) where

import Data.Array.Unboxed
import Data.Bits
import Euler.Util

pandigital :: Int -> Bool
pandigital x = p x 0 0
  where p :: Int -> Int -> Int -> Bool
        p 0 digits count = digits == 511
        p n digits count = let (n', d) = divMod n 10
                               digits' = digits .|. shiftL 1 (d-1)
                           in if digits == digits'
                              then False
                              else p n' digits' (count+1)

log10 = log 10
logPhi = log ((1 + sqrt 5) / 2) / log10
logRt5 = log (sqrt 5) / log10

isFibPan a b = pandigital a && pandigital b

findFibPan :: Int -> Int -> Int -> Int
findFibPan n1 n2 c
    | isFibPan first9 last9 = c
    | otherwise             = findFibPan n2 last9 (c+1)
    where first9 = let temp = fromIntegral c * logPhi - logRt5
                   in floor $ 10**(temp - fromIntegral (floor temp) + 9 - 1)
          last9 = (n1 + n2) `mod` 10^9

problem104 = NoInput . show $ findFibPan 1 1 3
