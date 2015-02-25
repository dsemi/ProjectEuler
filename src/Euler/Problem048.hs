module Euler.Problem048
( problem48
) where

import Euler.Util

problem48 = NoInputI $ sum [n^n `mod` 10^10 | n <- [1..1000]] `mod` (10^10)
