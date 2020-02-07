module Euler.Problem070
( problem70
) where

import Control.Arrow
import Control.Monad
import Data.List (sort,foldl')
import Data.Array.Unboxed
import Euler.Util
import Math.NumberTheory.ArithmeticFunctions

same (xs,ys) = xs' == ys'
    where xs' :: UArray Char Int
          xs' = accumArray (+) 0 ('0','9') . zip (show xs) $ repeat 1
          ys' :: UArray Char Int
          ys' = accumArray (+) 0 ('0','9') . zip (show ys) $ repeat 1


problem70 = NoInput . show . fst $ foldl' sift (87109,79180) [ (x, totient x)
                                                             | x <- [8000000..9999999]
                                                             , gcd x smallPrimes == 1 ]
    where smallPrimes = product [2,3,5,7,11,13,17,19,23,29]
          sift :: (Int, Int) -> (Int, Int) -> (Int, Int)
          sift ab cd = let ab' = uncurry (/) $ join (***) fromIntegral ab
                           cd' = uncurry (/) $ join (***) fromIntegral cd
                       in if cd' < ab' && same cd
                          then cd
                          else ab
