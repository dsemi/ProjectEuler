module Euler.Problem064
( problem64
) where

import Data.List.Ordered (minus)
import Euler.Util

flrt :: Integer -> Integer  -- flrt x ≈ √x,  with  flrt x^2 ≤ x < flrt(x+1)^2
flrt x = approx (round . (sqrt::Double->Double) . fromInteger $ x)
   where approx r
            | ctrl <= x, (r+1)^2 > x  = r
            | otherwise               = approx $ r - diff
          where ctrl = r^2
                diff = (ctrl - x) // (2*r)    -- ∂/∂x x² = 2x

         a//b = a`div`b + if (a>0)==(b>0) then 1 else 0   -- always away from 0


periodLength s = let m = 0
                     d = 1
                     a = a0
                 in findLen m d a 0
    where a0 = flrt s
          findLen m d a c
              | a == 2*a0 = c
              | otherwise = let m' = d*a - m
                                d' = (s - m'^2) `div` d
                                a' = (a0 + m') `div` d'
                            in findLen m' d' a' (c+1)

problem64 = NoInputI . length . filter odd . map periodLength $ [1..10000] `minus` scanl1 (+) [1,3..]
