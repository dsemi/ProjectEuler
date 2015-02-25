module Euler.Problem119
( problem119
) where

import Data.List (sort)
import Euler.Util

digs :: Integral x => x -> [x]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

problem119 = NoInputI $ sort [ n | s <- [2..200]
                             , p <- [2..9]
                             , let n = s^p
                             , s == sum (digs n) ] !! 29
