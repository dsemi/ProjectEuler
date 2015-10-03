module Euler.Problem018
( problem18
) where

import Euler.Util

problem18 = HasInput $ show . head . foldr1 reduce . map (map read . words) . lines
    where reduce a b = zipWith (+) a . zipWith max b $ tail b
