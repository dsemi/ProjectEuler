module Euler.Problem012
( problem12
) where

import Euler.Util
import Math.NumberTheory.ArithmeticFunctions

problem12 = NoInput . show $ head [x | x <- scanl1 (+) [1..], tau (x :: Int) > 500]
