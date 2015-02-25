module Euler.Problem439
( problem439
) where

import Data.List (group)
import Data.Numbers.Primes
import Data.Map.Lazy (fromList,(!))

triangle n = n*(n+1) `div` 2


divisorSummatory x = let u = flrt x
                     in (subtract (u^2)) . (*2) . sum $ map (x `div`) [1..u]

flrt :: Integer -> Integer
flrt x = approx (round . (sqrt::Double->Double) . fromInteger $ x)
   where approx r
            | ctrl <= x, (r+1)^2 > x  = r
            | otherwise               = approx $ r - diff
          where ctrl = r^2
                diff = (ctrl - x) // (2*r)

         a//b = a`div`b + if (a>0)==(b>0) then 1 else 0


sumDivisors n 
    | isPrime n = n+1
    | otherwise = product $ map (succ . sum . scanl1 (*)) (group $ primeFactors n)

divisorSumMap n = fromList $ map (\x -> (x,(sumDivisors x))) [1..n]

s n m  = foldr1 (\a b -> (a + b)) [m ! (a*b) | a <- [1..n], b <- [1..n]]

s' n = sum . map product . map (map (sumDivisors . foldl1 (*))) . map group $ map primeFactors [a*b | a <- [1..n], b <- [1..n]]

problem_439 = map s' [1..20]
