module Euler.Problem025
( problem25
) where

import Euler.Util

problem25 = NoInput . show $ head [x | x <- [1..], fibonacci x `div` 10^999 >= 1]
