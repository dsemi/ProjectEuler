module Euler.Problem086
( problem86
) where

import Euler.Util
import Math.NumberTheory.Powers

integerCuboidRoute m = sum [ if wh > m then m + 1 - (wh+1) `div` 2 else wh `div` 2 | wh <- [1..2*m]
                           , integerCuboid m wh ]
    where integerCuboid l wh = isSquare (l^2 + wh^2)

problem86 = NoInput . show $ findIntegerCuboidRoute (10^6) 0 0
    where findIntegerCuboidRoute lim m c
              | c >= lim = m
              | otherwise = findIntegerCuboidRoute lim (m+1) $ c + integerCuboidRoute (m+1)
