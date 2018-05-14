module Euler.Problem017
( problem17
) where

import qualified Data.Map as M
import Euler.Util

numToStr = M.fromList [ (0, ""), (1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five")
                      , (6, "six"), (7, "seven"), (8, "eight"), (9, "nine"), (10, "ten")
                      , (11, "eleven"), (12, "twelve"), (13, "thirteen"), (14, "fourteen")
                      , (15, "fifteen"), (16, "sixteen"), (17, "seventeen"), (18, "eighteen")
                      , (19, "nineteen"), (20, "twenty"), (30, "thirty"), (40, "forty"), (50, "fifty")
                      , (60, "sixty"), (70, "seventy"), (80, "eighty"), (90, "ninety")
                      , (100, "onehundred"), (200, "twohundred"), (300, "threehundred")
                      , (400, "fourhundred"), (500, "fivehundred"), (600, "sixhundred")
                      , (700, "sevenhundred"), (800, "eighthundred"), (900, "ninehundred")
                      , (1000, "onethousand") ]

getLen i
    | i <= 20          = length $ numToStr M.! i
    | i < 100          = length (numToStr M.! (i `div` 10 * 10)) + length (numToStr M.! (i `mod` 10))
    | i `mod` 100 == 0 = length $ numToStr M.! i
    | otherwise        = length (numToStr M.! (i `div` 100 * 100)) + 3 + getLen (i `mod` 100)

problem17 = NoInput . show $ sum $ map getLen [1..1000]
