module Euler.Problem037
( problem37
) where

import Data.List (tails,all)
import Math.NumberTheory.Primes.Testing
import Euler.Util

list n = filter isLeftT $ if isPrime n then n:ns else []
  where ns = concatMap (list . (10*n +)) [1,3,7,9]

isLeftT = all isPrime . map read . init . tail . tails . show

problem37 = NoInput . show . sum .  filter (>=10) $ concatMap list [2,3,5,7]
