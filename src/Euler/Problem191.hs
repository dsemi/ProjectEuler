module Euler.Problem191
( problem191
) where

import Euler.Util

p191 = f 1 0 0 0 0 0
  where f x2 x1 x0 y2 y1 y0 n
            | n == 0 = y3
            | otherwise = f x3 x2 x1 y3 y2 y1 $ n-1
            where x3 = x2 + x1 + x0
                  y3 = x3 + y2 + y1 + y0

problem191 = NoInput . show $ p191 30
