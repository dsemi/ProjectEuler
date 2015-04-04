module Euler.Problem026
( problem26
) where

import Euler.Util

longestReciprocal n = snd $ maximum $ map cycleLengthPair $ filter ((/=0) . flip mod 5) [3,5..n]
    where cycleLengthPair x = (length . takeWhile ((/=0) . flip mod x) $ map ((-1+) . (10^)) [1..], x)

problem26 = NoInput . show $ longestReciprocal 1000
