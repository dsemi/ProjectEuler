module Euler.Problem127
( problem127
) where

import Control.Monad
import Control.Monad.ST
import Data.Vector.Algorithms.Intro
import Data.Vector.Unboxed ((!),freeze,toList)
import qualified Data.Vector.Unboxed.Mutable as M
import Euler.Util

abcHit n = sum [ c | c <- [3..n]
               , let chalf = c `div` 2
               , (ra, a) <- takeWhile ((<=chalf) . (*) (rads ! c) . fst) $ toList sortedRads
               , a < chalf
               , let b = c - a
               , ra * (rads ! b) * (rads ! c) < c
               , gcd ra (rads ! c) == 1 ]
    where (rads,sortedRads) = runST $ do
            a <- M.replicate (n+1) 1
            forM_ [2..n] $ \p -> do
              cu <- M.read a p
              when (cu == 1) $ do
                forM_ [p,2*p..n] $ \v -> do
                  c <- M.read a v
                  M.write a v (c*p)
            a' <- M.new n
            forM_ [1..n] $ \i -> do
              r <- M.read a i
              M.write a' (i-1) (r,i)
            sort a'
            liftM2 (,) (freeze a) $ freeze a'


problem127 = NoInputI $ abcHit 120000

