module Euler.Problem036
( problem36
) where

import Data.Digits
import Euler.Util

palindrome :: (Eq a) => [a] -> Bool
palindrome xs = p [] xs xs
   where p rev (x:xs) (_:_:ys) = p (x:rev) xs ys
         p rev (x:xs) [_] = rev == xs
         p rev xs [] = rev == xs

problem36 = NoInputI $ sum [n | n <- [1..999999], palindrome (digits 10 n), palindrome (digits 2 n)]
