module Euler.Problem120
( problem120
) where

import Euler.Util

maxRem a = 2*a*((a-1) `div` 2)

problem120 = NoInputI . sum $ map maxRem [3..1000]
