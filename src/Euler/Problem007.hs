module Euler.Problem007
( problem7
) where

import Data.List.Ordered
import Euler.Util

primes = 2:3:([5,7..] `minus` unionAll [[p*p, p*p+2*p..] | p <- tail primes])
    where unionAll = foldr (\(x:xs)->(x:) . union xs) []

problem7 = NoInput . show $ primes !! 10000

