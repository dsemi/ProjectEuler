module Euler.Problem038
( problem38
) where

import Data.List (permutations,delete)
import Euler.Util

pandigitalsStartingWith n = map (fst . (foldr (\x (a,b) -> (a+x*b, b*10)) (n*10^8,1))) $ permutations (delete n [1..9])

problem38 = let testables = filter (>918273645) $ pandigitalsStartingWith 9
            in NoInput . show . maximum $ filter (\n -> (n `div` 10^5)*2 == n `mod` 10^5) testables
