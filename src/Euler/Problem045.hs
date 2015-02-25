module Euler.Problem045
( problem45
) where

import Euler.Util

hex n = n*(2*n-1)
hexes = map hex [1..]
isSquare num = num == (flrt num)^2

isTriangular num = let n = 8*num + 1
                   in isSquare n

isPentagonal num = let n = 24*num + 1
                   in isSquare n && flrt n `mod` 3 == 2

isHexagonal num = let n = 8*num + 1
                  in isSquare n && flrt n `mod` 4 == 3

problem45 = NoInputI . head . filter (\x -> isTriangular x && isPentagonal x) 
            $ dropWhile (<40756) hexes

flrt :: Integer -> Integer
flrt x = approx (round . (sqrt::Double->Double) . fromInteger $ x)
   where approx r
            | ctrl <= x, (r+1)^2 > x  = r
            | otherwise               = approx $ r - diff
          where ctrl = r^2
                diff = (ctrl - x) // (2*r)

         a//b = a`div`b + if (a>0)==(b>0) then 1 else 0

