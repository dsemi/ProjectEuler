module Euler.Problem031
( problem31
) where

import Euler.Util

coins = [200,100,50,20,10,5,2,1]

change :: Int -> [Int] -> Int
change 0 _  = 1
change n [] = 0
change _ [1] = 1
change n cns = let css = filter (<=n) cns
               in case css of
                    (c:cs) -> change (n-c) css + change n cs
                    [] -> 0

problem31 = NoInput . show $ change 200 coins
