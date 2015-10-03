module Euler.Problem062
( problem62
) where

import Data.List (group,sort,elemIndex)
import Data.Maybe
import Euler.Util

sortedCubeStrings = map (sort . show . (^3)) [0..10000]
digits = head . head . filter ((==5) . length) . group . sort $ sortedCubeStrings
Just num = elemIndex digits sortedCubeStrings

problem62 = NoInput . show $ toInteger num^3
