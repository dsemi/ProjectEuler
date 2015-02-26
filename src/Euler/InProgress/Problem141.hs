module Euler.Problem141
( problem141
) where

import Data.List (sort)
squares = scanl1 (+) [1,3..]

isProgressive n = or [a /= 0 && fromIntegral b^2/ fromIntegral a == fromIntegral c  | d <- [2..n-1]
                     , let (q,r) = n `divMod` d
                     , let [a,b,c] = sort [d,q,r]]
