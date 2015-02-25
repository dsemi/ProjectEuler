module Euler.Problem067
( problem67
) where

import Euler.Util

problem67 = HasInputI $ head . foldr1 reduce . map ((map read) . words) . lines
    where reduce a b = zipWith (+) a . zipWith max b $ tail b
