module Euler.Problem124
( problem124
) where

import Control.Monad
import Control.Monad.ST
import Data.Vector.Algorithms.Intro
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Euler.Util

rads n = runST $ do
            a <- M.replicate (n+1) (1,1)
            forM_ [2..n] $ \p -> do
              (cu,_) <- M.unsafeRead a p
              when (cu == 1) $ do
                forM_ [p,2*p..n] $ \v -> do
                  (c,_) <- M.unsafeRead a v
                  M.unsafeWrite a v (c*p,v)
            sort a
            M.unsafeRead a 10000

problem124 = NoInputI . snd $ rads 100000
