module Euler.Problem001
( problem1
) where

import Data.List.Ordered
import Euler.Util

combos xs = map (map product) . tail $ combosBySize xs
    where combosBySize = foldr f ([[]] : replicate (length xs) [])
          f x next     = zipWith (++) (map (map (x:)) ([]:next)) next

sumNumsUpToDivisibleBy n ms = sum . concat . zipWith map (cycle [(*1), (*(-1))]) . map (map f) 
                           . combos $ removeMultiples ms ms
    where removeMultiples [] ys = ys
          removeMultiples (x:xs) ys = removeMultiples xs $ ys `minus` [2*x, 3*x ..]
          f a = (n - n `mod` a) * ((n `div` a) + 1) `div` 2

problem1 = NoInputI $ sumNumsUpToDivisibleBy 999 [3, 5]
