module Euler.Problem243
( problem243
) where

import Data.Ratio
import Euler.Util
import Math.NumberTheory.ArithmeticFunctions
import Math.NumberTheory.Primes

problem243 = let limit = 15499 % 94744
                 rough = last . takeWhile (\x -> ratio x>limit) $ scanl1 (*) $ (map unPrime primes :: [Int])
             in NoInput . show . head . filter (\x -> ratio x < limit) $ map (*rough) [2..]
    where ratio d = totient d % (d-1)
