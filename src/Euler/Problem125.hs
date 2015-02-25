module Euler.Problem125
( problem125
) where

import Data.IntSet (IntSet, empty, union, toList, fromList)
import Euler.Util

palindrome :: (Eq a) => [a] -> Bool
palindrome xs = p [] xs xs
    where p rev (x:xs) (_:_:ys) = p (x:rev) xs ys
          p rev (x:xs) [_] = rev == xs
          p rev xs [] = rev == xs

problem125 = let n = 10^8
             in NoInputI . flip (pSums n) empty . takeWhile (<n) $ scanl1 (+) [1,3..]
    where pSums :: Int -> [Int] -> IntSet -> Int
          pSums _ [x] c        = sum $ toList c
          pSums n xss@(_:xs) c = pSums n xs . union c . fromList . filter (palindrome . show) . takeWhile (<n) . tail $ scanl1 (+) xss
