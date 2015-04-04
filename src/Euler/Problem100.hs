module Euler.Problem100
( problem100
) where

import Euler.Util

problem100 = NoInput . show . fst $ until (\(_,n) -> n>10^12) (\(b,n) -> (3*b+2*n-2, 4*b+3*n-3)) (15,21)
