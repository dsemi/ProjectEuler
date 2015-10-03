module Euler.Problem075
( problem75
) where

import Data.Array.Unboxed
import Euler.Util

pTriples :: [Int]
pTriples = [p |  n <- [1..floor (sqrt 1500000)]
           , m <- [n+1..floor (sqrt 1500000)]
           , even n || even m
           , gcd n m == 1
           , let a = m^2 - n^2
           , let b = 2*m*n
           , let c = m^2 + n^2
           , let p = a + b + c
           , p < 1500000 ]

problem75 = NoInput . show . length . filter (==1) $ elems a
    where a :: UArray Int Int
          a = accumArray (+) 0 (1,1500000) [ (p,1)
                                           | p <- [ n*p | p <- pTriples
                                                  , n <- [1..1500000 `div` p] ] ]
