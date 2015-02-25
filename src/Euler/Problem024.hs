module Euler.Problem024
( problem24
) where

import Data.List (delete)
import Euler.Util

f 0 = 1
f n = n * f (n-1)

permutations [] _ = []
permutations xs n = x : permutations (delete x xs) (mod n m)
  where m = f $ length xs - 1
        y = div n m
        x = xs !! y
 
problem24 = NoInputS $ permutations "0123456789" 999999
