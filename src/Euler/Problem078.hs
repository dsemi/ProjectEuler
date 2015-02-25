module Euler.Problem078
( problem78
) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Base
import Data.Array.Unboxed
import Data.STRef
import Euler.Util

partitions :: Int
partitions = runST $ do
               c <- newSTRef 0
               a <- newArray (0,1000000) 0 :: ST s (STUArray s Int Int)
               unsafeWrite a 0 1
               calcPartitions a [1..] c
               readSTRef c
    where signs :: [Int]
          signs = cycle [1,1,-1,-1]
          suite = concat [[(3*n^2-n) `div` 2,(3*n^2+n) `div` 2] | n <- [1..]]
          parts n = takeWhile (>= 0) [n-x| x <- suite]
          calcPartitions a (n:ns) c = do
            su <- liftM (`mod` 1000000) $ foldl1 (liftM2 (+)) [liftM (s*) (unsafeRead a p) | (s,p) <- zip signs $ parts n] 
            unsafeWrite a n su
            if su /= 0 then calcPartitions a ns c else writeSTRef c n

problem78 = NoInputI partitions
