module Euler.Problem009
( problem9
) where

import Euler.Util

problem9 = let x = 1000 
           in NoInput . show $ head [ a*b*c |  n <- [1,3..floor (sqrt $ fromIntegral x)]
                              , m <- [2,4..floor (sqrt $ fromIntegral x)]
                              , let a = abs (m^2 - n^2)
                              , let b = 2*m*n
                              , let c = m^2 + n^2
                              , let p = a + b + c
                              , p == x
                              ]
