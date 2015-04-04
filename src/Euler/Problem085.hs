module Euler.Problem085
( problem85
) where

import Euler.Util

problem85 = NoInput . show . snd $ minimum [ (abs (r - 2000000), x*y) | x <- [1..53]
                                     , y <- [1..2000]
                                     , let r = x*y*(x+1)*(y+1) `div` 4 ]

