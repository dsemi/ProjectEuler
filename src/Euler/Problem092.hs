module Euler.Problem092
( problem92
) where

import Data.Array (array,(!))
import Data.Char (intToDigit)
import Data.List (group)
import Euler.Util

makeIncreas 1 minnum  = [[a] | a <- [minnum..9]]
makeIncreas digits minnum  = [a:b | a <- [minnum..9], b <- makeIncreas (digits-1) a]
squares = array ('0','9') [(intToDigit x,x^2) | x <- [0..9]]
 
next :: Int -> Int
next = sum . map (squares !) . show

factorial = (map f [0..] !!)
    where f 0 = 1
          f n = n*factorial (n-1)

countNum xs = product . map (factorial . length) $ group xs

yield :: Int -> Int
yield = until (\x -> x == 89 || x == 1) next

problem92 = NoInputI $ sum [ div p7 (countNum a) | a <- tail $ makeIncreas 7 0
                           , yield (sum $ map (^2) a) == 89]
    where p7 = factorial 7
