module Euler.Problem077
( problem77
) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.List (find)
import Euler.Util

sieveUA :: Int -> UArray Int Bool
sieveUA top = runSTUArray $ do
    let m = (top-1) `div` 2
        r = floor . sqrt $ fromIntegral top + 1
    sieve <- newArray (1,m) True      -- :: ST s (STUArray s Int Bool)
    forM_ [1..r `div` 2] $ \i -> do
      isPrime <- readArray sieve i
      when isPrime $               -- ((2*i+1)^2-1)`div`2 == 2*i*(i+1)
        forM_ [2*i*(i+1), 2*i*(i+2)+1..m] $ \j ->
          writeArray sieve j False
    return sieve

primesToUA :: Int -> [Int]
primesToUA top = 2 : [i*2+1 | (i,True) <- assocs $ sieveUA top]

counter = foldl (\without p -> let (poor,rich) = splitAt p without
                                   with =  poor ++ zipWith (+) with rich
                               in with) (1 : repeat 0)

problem77 = let (Just x) = find ((>5000) . (ways !!)) [1..]
            in NoInput $ show x
    where ways = counter $ primesToUA 1000
