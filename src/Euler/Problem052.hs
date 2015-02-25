module Euler.Problem052
( problem52
) where

import Data.List (sort)
import Euler.Util

-- Apparently this makes almost no difference on the time than going full-on brute force
l = concatMap ranges [1..]
    where ranges x = let o = 10^x
                         n = pred . length $ show o
                     in [o..read ('1':replicate n '6') :: Integer]

problem52 = NoInputI $ head [ a | a <- l
                            , let a1 = sort . show $ a
                            , let a2 = sort . show $ a*2
                            , let a3 = sort . show $ a*3 
                            , let a4 = sort . show $ a*4
                            , let a5 = sort . show $ a*5
                            , let a6 = sort . show $ a*6
                            , a1 == a2
                            , a1 == a3
                            , a1 == a4
                            , a1 == a5
                            , a1 == a6 ]
