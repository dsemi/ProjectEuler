module Euler.Problem091
( problem91
) where

import Control.Arrow
import Control.Monad
import Data.Ix
import Euler.Util

count f ini cond = cnt ini 0
    where cnt v c
              | cond v     = cnt (f v) (c+1)
              | otherwise  = c

numRightTriangles lim = sum [ validPts + if y == 0 then lim else 0 | pt <- tail $ range ((0,0),(lim,lim))
                            , let g = uncurry gcd pt
                            , let (x,y) = join (***) (`div` g) pt
                            , let f1 (a,b) = (a+y,b-x)
                            , let f2 (a,b) = (a-y,b+x)
                            , let validPts = count f1 pt (uncurry (&&) . ((<=lim) *** (>=0))) +
                                             count f2 pt (uncurry (&&) . ((>=0) *** (<=lim))) - 2 ]

problem91 = NoInputI $ numRightTriangles 50
