module Euler.Problem115
( problem115
) where

import Euler.Util

n `choose` x = product [n-x+1..n] `div` product [2..x]

numCombos m n = sum [(k+a) `choose` (k-a) | a <- [0..(n+1) `div` (m+1)], let k = 1-a*m+n]

problem115 = NoInput . show . head $ dropWhile ((<10^6) . numCombos 50) [1..]
