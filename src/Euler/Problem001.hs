module Euler.Problem001
( problem1
) where

import Euler.Util

problem1 = NoInputI $ sum [a | a <- [1..999], a `mod` 3 == 0 || a `mod` 5 == 0]
