module Euler.Problem051
( problem51
) where

import Data.Maybe
import Data.String.Utils
import Data.List (maximumBy)
import Data.Function (on)
import Euler.Util
import Math.NumberTheory.Primes (unPrime)
import Math.NumberTheory.Primes.Sieve
import Math.NumberTheory.Primes.Testing

-- Looking for 6 digit prime with 3 repeated digits
-- Only need to look for 0,1, or 2 because 8 of them repeat

-- Moved bound up to 111858 b/c 111857 yields true from 857
primeNums = map (show . unPrime) $ sieveFrom 111858

checkPrime p
    | isJust num = let n      = [fromJust num]
                       family = map (read . (\x -> replace n x p)) repls
                   in length (filter isPrime family) == 8
    | otherwise  = False
    where poss  = map ($ p) [filter (=='0'), filter (=='1'), filter (=='2')]
          x     = maximumBy (compare `on` length) poss
          num   = if length x > 2 then Just (head x) else Nothing
          repls = map (:[]) ['0'..'9']

problem51 = NoInput . head $ filter checkPrime primeNums
