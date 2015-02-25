module Euler.Problem477
( problem477
) where

import qualified Data.Vector as V
import Data.Function.Memoize

nums n = V.iterateN n ((`mod` 1000000007) . (+45) . (^2)) 0


f = memoize f'
    where f' :: V.Vector Int -> Int
          f' s
              | V.null s  = 0
              | otherwise = V.sum s + max (negate $ f $ V.tail s) (negate $ f $ V.init s)
