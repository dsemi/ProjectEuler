module Euler.Problem044
( problem44
) where

import Euler.Util
import Math.NumberTheory.Powers.Squares

isPentagonal num = let n = 24*num + 1
                   in isSquare n && integerSquareRoot n `mod` 3 == 2

problem44 = NoInputI $ head [ a'-b' | a <- [1..], b <- [1..a]
                            , let a' = a*(3*a-1) `div` 2
                            , let b' = b*(3*b-1) `div` 2
                            , isPentagonal (a'-b')
                            , isPentagonal (a'+b') ]
