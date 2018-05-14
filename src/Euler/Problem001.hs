module Euler.Problem001
( problem1
) where

import Euler.Util
import Data.List.Ordered

problem1 = NoInput . show $ sum $ takeWhile (<1000) $ unionAll $ map (\x -> [x, x+x ..]) [3, 5]
