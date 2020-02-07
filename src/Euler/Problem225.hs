module Euler.Problem225
    ( problem225
    ) where

import Euler.Util


dividesTribonacci :: Int -> Bool
dividesTribonacci n = go 1 1 $ 3 `mod` n
    where go 1 1 1 = False
          go _ _ 0 = True
          go a b c = go b c $ (a + b + c) `mod` n

problem225 = NoInput $ show $ (!! 123) $ filter (not . dividesTribonacci) [1, 3..]
