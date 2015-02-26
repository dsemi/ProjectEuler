module Euler.Problem146
( problem146
) where

import Math.NumberTheory.Primes.Testing

squares = drop 2 $ scanl1 (+) [1,3..10000^2]

-- Difference beteween first two in sequence is 2, find twin primes and see if first -1 is square number

problem_146 = [flrt n | n <- squares, all isPrime . map (n +) $ [1,3,7,9,13,27]]

flrt :: Integer -> Integer
flrt x = approx (round . (sqrt::Double->Double) . fromInteger $ x)
   where approx r
            | ctrl <= x, (r+1)^2 > x  = r
            | otherwise               = approx $ r - diff
          where ctrl = r^2
                diff = (ctrl - x) // (2*r)

         a//b = a`div`b + if (a>0)==(b>0) then 1 else 0
