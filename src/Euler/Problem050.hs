module Euler.Problem050
( problem50
) where

import Data.Ix (range)
import Data.Array (listArray,bounds,(!))
import Euler.Util
import Math.NumberTheory.Primes

primeSums n ll = let l = scanl1 (+) $ takeWhile (<n) primes
                     i = foldr (\x acc -> if (l !! x) - (l !! (x-ll)) >= n then x else acc) (length l - 1) [ll..length l - 1]
                 in listArray (0,i) l

problem50 = let n        = 1000000
                primeSum = primeSums n 393
            in NoInputI . snd . maximum $ [ (a-b,s) | a <- range (bounds primeSum)
                                          , b <- [0..a]
                                          , let s = (primeSum ! a) - (primeSum ! b)
                                          , s < n
                                          , isPrime s]


