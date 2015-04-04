module Euler.Problem076
( problem76
) where

import Data.Array
import Euler.Util

partitions :: Array Int Integer
partitions = 
    array (0,1000000) $ 
    (0,1) : 
    [(n,sum [s * partitions ! p | (s,p) <- zip signs $ parts n]) | n <- [1..1000000]]
    where
        signs = cycle [1,1,-1,-1]
        suite = map penta $ concat [[n,-n]|n <- [1..]]
        penta n = n*(3*n - 1) `div` 2
        parts n = takeWhile (>= 0) [n-x| x <- suite]

problem76 = NoInput . show $ partitions ! 100 - 1
