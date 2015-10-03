module Euler.Problem181
( problem181
) where

import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed
import Euler.Util

partitions b w = let dpArray :: UArray (Int,Int) Int
                     dpArray = runSTUArray $ do
                       a <- newArray ((0,0), (b,w)) 0
                       writeArray a (0,0) 1
                       let nums = tail [(i,j) |  i <- [0..b], j <- [0..w]]
                       forM_ nums $ \(i,j) ->
                           forM_ [i..b] $ \k ->
                               forM_ [j..w] $ \l ->
                                   liftM2 (+) (readArray a (k,l)) (readArray a (k-i,l-j))
                                             >>= writeArray a (k,l)
                       return a
                 in dpArray ! (b,w)

problem181 = NoInput . show $ partitions 60 40
