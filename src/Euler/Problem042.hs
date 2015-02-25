module Euler.Problem042
( problem42
) where

import Data.List.Split
import Data.Char (ord)
import Euler.Util

-- Must be capital letter
getCharCode l = fromIntegral $ ord l - 64

getWordValue n = sum $ map getCharCode n

isTriangular num = let n = flrt $ num*2
                   in n*(n+1) `div` 2 == num

p42 x = let l    = splitOn "," $ filter (/='\"') x
            nums = map getWordValue l
        in length . filter isTriangular $ nums

flrt :: Integer -> Integer
flrt x = approx (round . (sqrt::Double->Double) . fromInteger $ x)
   where approx r
            | ctrl <= x, (r+1)^2 > x  = r
            | otherwise               = approx $ r - diff
          where ctrl = r^2
                diff = (ctrl - x) // (2*r)

         a//b = a`div`b + if (a>0)==(b>0) then 1 else 0

problem42 = HasInputI p42
