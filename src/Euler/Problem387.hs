module Euler.Problem387
( problem387
) where

import Euler.Util
import Math.NumberTheory.Primes

sumDigits n = f n 0
    where f 0 c = c
          f x c = let (a,b) = x `divMod` 10
                  in f a $ c + b

isHarshad n = n `mod` sumDigits n == 0

isStrong n = isPrime $ n `div` sumDigits n

recursiveHarshads = createHarshads $ filter isHarshad [10..99]
    where createHarshads seeds = seeds ++ createHarshads (filter isHarshad $ concatMap addAllDigits seeds)

addAllDigits n = map (+ 10*n) [0..9]

primeHarshadsFrom = filter isPrime . addAllDigits

problem387 = NoInput . show $ sum . takeWhile (< 10^14) . concatMap primeHarshadsFrom $ filter isStrong recursiveHarshads
