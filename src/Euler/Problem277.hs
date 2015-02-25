module Euler.Problem277
( problem277
) where

import Data.List (foldl')
import Data.Map (fromList,(!))
import Euler.Util

pattern = map (m !) "UDDDUdddDDUDDddDdDddDDUDDdUUDd"
    where m = fromList [('D',0),('U',1),('d',2)]

problem277 = NoInputI $ mCollatz (10^15) 1 1
    where mCollatz n pm i = let seq     = map (`mod` 3) $ iterate nextMCollatz n
                                (cp,nm) = foldl' cmp (True,0) $ zip pattern seq
                            in if cp then n
                               else if nm > pm then
                                        mCollatz (n+3*i) nm (3*i)
                                    else
                                        mCollatz (n+i) pm i
          cmp (p,c) (a,b) = if p
                            then (a==b,c+1) 
                            else (p,c)
          nextMCollatz n
              | m == 0    = n `div` 3
              | m == 1    = (4*n + 2) `div` 3
              | otherwise = (2*n - 1) `div` 3
              where m     = n `mod` 3

