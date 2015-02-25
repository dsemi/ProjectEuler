module Euler.Problem027
( problem27
) where

import Euler.Util
import Math.NumberTheory.Primes

quadratic a b n = n^2 + a*n + b

pairs n = [(a,b) | b <- takeWhile (<n) primes, a <- filter odd [-b+1..n-1], a+b>0]

numPrimesGenerated (a,b) = length . takeWhile isPrime $ map (quadratic a b) [0..]

problem27 = NoInputI . snd . maximum . map (\(a,b) -> (numPrimesGenerated (a,b), a*b)) $ pairs 1000

