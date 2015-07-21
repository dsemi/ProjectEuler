{-# LANGUAGE FlexibleContexts #-}

module Euler.Problem095
( problem95
) where

import Data.Tuple
import Control.Monad
import Data.Array.ST
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.Unboxed
import Data.List (elemIndex, foldl1', maximumBy)
import Euler.Util

findChainLens :: UArray Int Int
findChainLens = runSTUArray $ do
                  a <- newArray (0,1000000) (-1)
                  forM_ [1..1000000] $ \n -> findLen a n []
                  return a
    where findLen a n c
              | n > 1000000 || n == 1 = forM_ c $ \e -> unsafeWrite a e 0
              | otherwise = case rep of
                              (Just l) -> forM_ (take (l+1) c) $ \e -> unsafeWrite a e l
                              _        -> do
                                       l <- unsafeRead a n
                                       if l /= (-1)
                                       then forM_ c $ \e -> unsafeWrite a e 0
                                       else findLen a (sumDivisors ! n) (n:c)
              where rep = elemIndex n c

sumDivisors :: UArray Int Int
sumDivisors = runSTUArray $ do
                    a <- newArray (0,1000000) 1
                    forM_ [2..1000] $ \i -> do
                      k <- unsafeRead a (i*i)
                      unsafeWrite a (i*i) (k+i)
                      forM_ (takeWhile ((<=1000000) . (*i)) [i+1..]) $ \j -> do
                        c <- unsafeRead a (i*j)
                        unsafeWrite a (i*j) (c+i+j)
                    return a

cmp :: (Int,Int) -> (Int,Int) -> Ordering
cmp (a,b) (c,d) 
    | b < d     = LT
    | b > d     = GT
    | a < c     = GT
    | a > c     = LT
    | otherwise = EQ

problem95 = NoInput . show . fst . maximumBy cmp $ assocs findChainLens
