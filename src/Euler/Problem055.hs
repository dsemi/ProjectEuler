module Euler.Problem055
( problem55
) where

import Euler.Util

reverseNum' 0 b = b
reverseNum' a b = reverseNum' (a `div` 10) (b*10 + (a `mod` 10))

revNum a = reverseNum' a 0

palindrome :: (Eq a) => [a] -> Bool
palindrome xs = p [] xs xs
   where p rev (x:xs) (_:_:ys) = p (x:rev) xs ys
         p rev (x:xs) [_] = rev == xs
         p rev xs [] = rev == xs


lychrel n = f (n + revNum n) 0
    where f _ 51 = True
          f n i | palindrome $ show n = False
                | otherwise = f (n + revNum n) $ succ i

problem55 = NoInputI . length $ filter lychrel [1..9999]
