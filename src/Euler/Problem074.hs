module Euler.Problem074
( problem74
) where

import Data.Array
import Data.List (foldl')
import Data.Char (digitToInt)
import qualified Data.Set as S
import qualified Data.Array.Unboxed as U
import Euler.Util

f :: U.UArray Int Int
f = U.listArray (0,9) $ 1: scanl1 (*) [1..]

sumDigitFacts :: Int -> Int
sumDigitFacts n = sdf n 0
    where sdf 0 c = c
          sdf n c = sdf (n `div` 10) (c + f U.! (n `mod` 10))

chainLens :: Int -> Array Int Int
chainLens n = a
    where a = listArray (1,n) $ 1:[cLen x 1 (S.singleton x) | x <- [2..n]]
          cLen n' c s -- cuts off at 60
              | n' == 145       = 1
              | n' == 169       = 3
              | n' == 871       = 2
              | n' == 872       = 2
              | c > 60          = c
              | S.member next s = c
              | next <= n       = c + a ! next
              | otherwise       = cLen next (c+1) $ S.insert next s
              where next = sumDigitFacts n'

problem74 = NoInputI . foldl' (\c x -> if x == 60 
                                       then c+1 
                                       else c) 0 . elems $ chainLens 999999
