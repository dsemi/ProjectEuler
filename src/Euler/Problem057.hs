module Euler.Problem057
( problem57
) where

import Data.Ratio
import Euler.Util

sqrtIterations = map (+1) $ iterate (recip . (+2)) (1%2)

problem57 = NoInput . show . length . filter nGTd $ take 1000 sqrtIterations
    where nGTd r = (length . show . numerator) r > (length . show . denominator) r
