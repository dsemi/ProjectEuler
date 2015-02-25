module Euler.Problem114
( problem114
) where

import Euler.Util

n `choose` x = product [n-x+1..n] `div` product [2..x]

numCombos m n = sum [(k+a) `choose` (k-a) | a <- [0..(n+1) `div` (m+1)], let k = 1-a*m+n]

problem114 = NoInputI $ numCombos 3 50
