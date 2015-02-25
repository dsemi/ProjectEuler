module Euler.Problem117
( problem117
) where

import Euler.Util

f 0 = 1
f n = n * f (n-1)

combos x = [[r,g,bl,b] | 
            r  <- [0..x `div` 2],
            g  <- [0..x `div` 3],
            bl <- [0..x `div` 4],
            b  <- [0..x],
            2*r+3*g+4*bl+b == x]

problem117 = NoInputI . sum . map (\xs -> f (sum xs) `div` (product $ map f xs)) $ combos 50
