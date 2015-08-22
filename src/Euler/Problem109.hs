module Euler.Problem109
( problem109
) where

import Euler.Util

problem109 = let all_poss = [1..20] ++ [2,4..40] ++ [3,6..60] ++ [0,25,50]
                 doubles = 50 : [2,4..40]
             in NoInput . show $ length [ 1
                                        | (i,a) <- zip [0..] all_poss
                                        , b <- drop i all_poss
                                        , c <- doubles
                                        , a + b + c < 100 ]
