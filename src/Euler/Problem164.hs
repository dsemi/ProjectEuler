module Euler.Problem164
( problem164
) where

import qualified Data.HashMap.Strict as M
import Euler.Util

poss = [10*x+y | x <- [0..9], y <- [0..9], x+y <= 9]

m :: M.HashMap Int [Int]
m = M.fromList [ (i,indices) | i <- poss
               , let (f,s)   = i `divMod` 10
               , let indices = [ x | x <- poss
                               , let (a,b) = x `divMod` 10
                               , b == f
                               , s + a + b <= 9 ] ]
           
nextState :: M.HashMap Int Int -> M.HashMap Int Int
nextState m' = M.fromList [ (i, next) | i <- poss
                          , let next = sum . map (\x -> M.lookupDefault 0 x m') $ m M.! i ]

problem164 = let n = 20
                 initial = M.fromList . zip [1..9] $ repeat 1
             in NoInputI . sum . M.elems . head . drop (n-1) $ iterate nextState initial

-- find numbers in poss that end with the number that you start with and whose sum will add to the number that you end with and be <= 9
