module Euler.Problem041
( problem41
) where

import Data.List (permutations)
import Math.NumberTheory.Primes (isPrime)
import Euler.Util

problem41 = NoInputI . maximum . filter isPrime . map read 
            $ permutations ['1'..'7'] ++ permutations ['1'..'4']
