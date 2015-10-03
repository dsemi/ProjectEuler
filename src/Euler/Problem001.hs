module Euler.Problem001
( problem1
) where

import Euler.Util

import Data.List (foldl')
import Data.List.Ordered

combos xs = map (map product) . tail $ combosBySize xs
    where combosBySize = foldr f ([[]] : replicate (length xs) [])
          f x next     = zipWith (++) (map (map (x:)) ([]:next)) next

sumNumsUpToDivisibleBy n ms = sum . concat . zipWith map (cycle [(*1), (*(-1))]) . map (map f)
                           . combos $ removeMultiples ms
    where removeMultiples xs = foldl' (\ys x -> ys `minus` [2*x, 3*x ..]) xs xs
          f a = (n - n `mod` a) * ((n `div` a) + 1) `div` 2

problem1 = NoInput . show $ sumNumsUpToDivisibleBy 999 [3, 5]
