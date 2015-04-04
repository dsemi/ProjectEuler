module Euler.Problem002
( problem2
) where

import Euler.Util

fib = 1:1:zipWith (+) fib (tail fib)

problem2 = NoInput . show . sum . filter even $ takeWhile (<4*10^6) fib
