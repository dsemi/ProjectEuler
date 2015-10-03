module Euler.Problem053
( problem53
) where

import Euler.Util

n `choose` x = product [n-x+1..n] `div` product [2..x]

numOfCombosMoreThan1Mil n
    | odd n  = 2 * foldl (\acc x-> if n `choose` x > 10^6 then acc+1 else acc) 0 [0..(n `div` 2)]
    | even n = 2 * foldl (\acc x-> if n `choose` x > 10^6 then acc+1 else acc) 0 [0..(n `div` 2 - 1)] + if n `choose` (n `div` 2) > 10^6 then 1 else 0

problem53 = NoInput . show . sum . map numOfCombosMoreThan1Mil $ [1..100]
