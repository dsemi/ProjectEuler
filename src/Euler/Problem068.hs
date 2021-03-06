module Euler.Problem068
( problem68
) where

import Data.List (permutations)
import Euler.Util

problem68 = let poss = filter ((==6) . head) $ permutations [1..10]
            in NoInput . concatMap show . maximum . map (\x -> [head x, x !! 1, x !! 2, x !! 3, x !! 2, x !! 4, x !! 5, x !! 4, x !! 6, x !! 7, x !! 6, x !! 8, x !! 9, x !! 8, x !! 1]) $ filter (\x -> head x + x !! 1 + x !! 2 ==  14 && x !! 3 + x !! 2 + x !! 4 ==  14 && x !! 5 + x !! 4 + x !! 6 ==  14 && x !! 7 + x !! 6 + x !! 8 ==  14 && x !! 9 + x !! 8 + x !! 1 == 14) poss

