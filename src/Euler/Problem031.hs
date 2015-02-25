module Euler.Problem031
( problem31
) where

import Data.List (permutations,inits)
import Data.Map (fromList, (!), union, member, empty)
import Euler.Util

coins = [1,2,5,10,20,50,100,200]

memo n m chs = m `union` (fromList . foldl (\acc x -> ((n,x), countCoins n x m):acc) [] $ inits chs)

countCoins 0 _      m = 1
countCoins _ []     m = 0
countCoins _ [1]    m = 1 -- Shaves off a decent chunk of time
countCoins n xs     m
    | n < 0           = 0
    | member (n,xs) m = m ! (n,xs)
    | otherwise       = countCoins n (init xs) m + countCoins (n - last xs) xs m

problem31 = let m = foldl (\acc x -> memo x acc coins) empty coins
            in NoInputI $ m ! (200,coins)
