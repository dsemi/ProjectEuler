module Euler.Problem003
( problem3 
) where

import Data.List (unfoldr)
import Euler.Util

smallestDivisor n = head $ filter ((==0) . (n `mod`)) [2..]

nextFactor 1 = Nothing
nextFactor n = let m = smallestDivisor n
               in Just (m, n `div` m)

factorize = unfoldr nextFactor

problem3 = NoInput . show . last $ factorize 600851475143
