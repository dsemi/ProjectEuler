module Euler.Problem443
( problem443
) where

import Euler.Util
import Math.NumberTheory.Primes.Factorisation

diff (a,b) n ans
    | a >= n = ans
    | otherwise = let m = map fst $ factorise (b-1)
                      r = minimum . zipWith (*) m $ map (succ . (a `div`)) m 
                  in diff (r,b + gcd r (b-1) - 1) n (b+n)

problem443 = let n = 10^15
             in NoInput . show $ diff (6,10) n 0
