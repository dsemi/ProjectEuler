module Euler.Problem097
( problem97
) where

import Euler.Util

problem97 = NoInput . show $ (28433*2^7830457 + 1) `mod` 10^10
