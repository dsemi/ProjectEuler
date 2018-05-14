module Euler.Problem108
( problem108
) where

-- ny + nx = xy
-- nx - xy + ny = 0
-- y = n*x/(x-n)

-- x > n
-- when x = y, x = 2n
-- 1 <= (x-n) <= n
-- This means number of solutions must be less than n
-- Solution maximized when n*x is divisible by most numbers between 1 and n

import Math.NumberTheory.ArithmeticFunctions
import Euler.Util

numSolutions :: Int -> Int
numSolutions n = (tau (n^2) + 1 ) `div` 2

problem108 = NoInput . show . head $ dropWhile ((<=1000) . numSolutions) [4..]
