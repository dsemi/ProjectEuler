module Euler.Problem104
( problem104
) where

import Data.Array.Unboxed
import Euler.Util

pandigital xs = and $ elems arr
 where arr :: UArray Char Bool
       arr = accumArray (||) False ('0','9') $
              map (flip (,) True) $ '0':xs

isFibPan n = let a = show $ n `mod` 10^9
                 b = take 9 $ show n
             in pandigital a && pandigital b

findFibPan n1 n2 c 
    | isFibPan s = c
    | otherwise = findFibPan n2 s (c+1)
    where s = n1+n2

problem104 = NoInputI $ findFibPan 1 1 3
