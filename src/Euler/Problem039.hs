module Euler.Problem039
( problem39
) where

import Euler.Util

import Control.Arrow
import Data.List (group,sort)
import Data.Tree

pTriples n = unfoldTree nextTriplets first
    where first = (3,4,5)
          fA (a,b,c) = (a - 2*b + 2*c, 2*a - b + 2*c, 2*a - 2*b + 3*c)
          fB (a,b,c) = (a + 2*b + 2*c, 2*a + b + 2*c, 2*a + 2*b + 3*c)
          fC (a,b,c) = ((-a) + 2*b + 2*c, (-2)*a + b + 2*c, (-2)*a + 2*b + 3*c)
          nextTriplets x = (x, filter (\(a,b,c) -> a+b+c <= n) [fA x, fB x, fC x])

problem39 = NoInput . show . snd . maximum . map (length &&& head) . group . sort
            $ [ n*p | p <- map add $ flatten $ pTriples 1000
              , n <- [1..1000 `div` p]]
    where add (a,b,c) = a+b+c
