module Euler.Problem073
( problem73
) where

import Control.Parallel.Strategies
import Euler.Util

farey :: (Int,Int) -> (Int,Int) -> Int -> Int -> Int
farey a@(na,da) b@(nb,db) c c'
    | da + db <= c = left
    | otherwise    = c'
    where mediant  = (na + nb,da + db)
          right    = farey a mediant c 0
          left     = farey mediant b c (1 + c' + right)

problem73 = NoInputI $ farey (1,3) (1,2) 12000 0
