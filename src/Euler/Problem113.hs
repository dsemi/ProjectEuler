module Euler.Problem113
( problem113
) where

import Euler.Util

n `choose` x = product [n-x+1..n] `div` product [1..x]
numNonBouncies n = ((n+10) `choose` 10) + ((n+9) `choose` 9) - 2 - 10*n

problem113 = NoInput . show $ numNonBouncies 100
