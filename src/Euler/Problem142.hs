{-# OPTIONS_GHC -O2 #-}

module Euler.Problem142
( problem142
) where

import Euler.Util
import Math.NumberTheory.Powers.Squares

problem142 = NoInput . show $ head [ x+y+z | a <- squares
                             , c <- takeWhile (<a) squares
                             , let f = a-c
                             , f > 0 && isSquare' f
                             , d <- takeWhile (<c) squares
                             , odd c == odd d
                             , let e = a-d
                             , let b = c-e
                             , b > 0 && e > 0 && isSquare' b && isSquare' e
                             , let x = (a+b) `div` 2
                             , let y = (e+f) `div` 2
                             , let z = (c-d) `div` 2 ]
    where squares = scanl1 (+) [1,3..]

