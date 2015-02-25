module Euler.Problem069
( problem69
) where

import Data.List (inits)
import Euler.Util
import Math.NumberTheory.Primes

{- Old Slow way
import Data.List (nub,foldl')
import Data.Ratio

totient n = numerator $ fromIntegral n * product [1 - 1%p | p <- nub (primeFactors n)]
maxRatioNToPhi m n = max m (n % totient n,n)
main = print . snd $ foldl' maxRatioNToPhi (0,0) [2..1000000]
-}

problem69 = NoInputI . maximum . takeWhile (<=10^6) . map product . tail $ inits primes
