{-# LANGUAGE BangPatterns #-}

module Euler.Problem129
( problem129
) where

import Euler.Util

a n = findDiv 1 1
    where findDiv !r !k
              | r == 0    = k
              | otherwise = findDiv ((10*r+1) `mod` n) (k+1)

problem129 = NoInputI $ head [n | n <- [1000001,1000003..], n `mod` 5 /= 0, a n > 1000000]
