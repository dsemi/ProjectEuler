module Euler.Problem112
( problem112
) where

import Data.Char (digitToInt)
import Euler.Util

isBouncy n = let n'    = map digitToInt $ show n
                 pairs = zip n' $ tail n'
             in not $ all (uncurry (<=)) pairs || all (uncurry (>=)) pairs

_countBouncy b n p
    | fromIntegral b / fromIntegral n >= p = n
    | isBouncy (n+1) = _countBouncy (b+1) (n+1) p
    | otherwise      = _countBouncy b (n+1) p

countBouncyUpTo = _countBouncy 0 1

problem112 = NoInput . show $ countBouncyUpTo 0.99
