module Euler.Problem028
( problem28
) where

import Data.List (concatMap)
import Euler.Util

diags n = takeWhile (<=n) . scanl (+) 1 $ concatMap (replicate 4) [2,4..]

problem28 = NoInput . show . sum $ diags (1001^2)
