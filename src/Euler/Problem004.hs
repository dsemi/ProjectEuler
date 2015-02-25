module Euler.Problem004
( problem4
) where

import Euler.Util

palindrome :: (Eq a) => [a] -> Bool
palindrome xs = p [] xs xs
    where p rev (x:xs) (_:_:ys) = p (x:rev) xs ys
          p rev (x:xs) [_] = rev == xs
          p rev xs [] = rev == xs

problem4 = NoInputI $ maximum [ p | x <- [100..999], y <- [x..999]
                              , x `mod` 11 == 0 || y `mod` 11 == 0
                              , let p = x*y
                              , palindrome $ show p]
