module Euler.Problem401
( problem401
) where

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
