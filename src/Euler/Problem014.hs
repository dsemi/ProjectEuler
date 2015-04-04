module Euler.Problem014
( problem14
) where

import Data.Bits
import Data.List (foldl1')
import Euler.Util

collatzLen :: Int -> Int -> Int
collatzLen _ 0 = 0
collatzLen c 1 = c+1
collatzLen c n
    | testBit n 0  = collatzLen (c+2) (shiftR (3*n + 1) 1)
    | otherwise    = collatzLen (c+1) (shiftR n 1)

problem14 = NoInput . show . snd $ foldl1' max [(collatzLen 0 x,x) | x <- [500000..1000000]]
