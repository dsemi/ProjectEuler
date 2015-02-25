module Euler.Problem116
( problem116
) where

import Euler.Util

n `choose` x = product [n-x+1..n] `div` product [2..x]

combos x ratio = [[a,b] | a <- [1..x], b <- [0..x], ratio*a+b == x]

problem116 = NoInputI . sum . map (\x -> sum x `choose` head x) 
             $ (combos 50 2 ++ combos 50 3 ++ combos 50 4)

