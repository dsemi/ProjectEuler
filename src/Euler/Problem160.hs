module Euler.Problem160
( problem160
) where

import Data.Array
import Data.List.Ordered
import Euler.Util
import Math.NumberTheory.Powers
import Math.NumberTheory.Logarithms

n = 10^12
lim = 10^5

f p i = if i `mod` 2 /= 0 && i `mod` 5 /= 0 
        then p*i
        else p

phis :: Array Integer Integer
phis = listArray (0,lim) $ scanl f 1 [1..lim]

result = product [ phis ! (n `div` p `mod` lim) `mod` lim | p2 <- [0 .. integerLogBase 2 n]
                 , p <- takeWhile (<=n) $ map ((2^p2*) . (5^)) [0 .. integerLogBase 5 n]
                 ] `mod` lim

num i = ps n i 0
    where ps x f c
              | x' == 0   = c
              | otherwise = ps x' f $ c+x'
              where x' = x `div` f
 
problem160 = NoInputI $ result * powerMod 2 (num 2 - num 5) lim `mod` lim
