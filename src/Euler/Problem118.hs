module Euler.Problem118
( problem118
) where

import Control.Lens
import Data.List (permutations)
import Euler.Util
import Math.NumberTheory.Primes

digitsToInt :: [Int] -> Int
digitsToInt = f 0
    where f c []     = c
          f c (x:xs) = f (10*c + x) xs

checkPermutations :: [[Int]] -> Int
checkPermutations = sum . map (cp 0 9) 
    where cp :: Int -> Int -> [Int] -> Int
          cp _ _ [] = 1
          cp p l xs = sum [ cp next (l-n) rest | n <- [1..l]
                          , let (next,rest) = _1 %~ digitsToInt $ splitAt n xs
                          , next > p
                          , isPrime $ toInteger next ]

problem118 = NoInput . show . checkPermutations $ permutations [1..9]
