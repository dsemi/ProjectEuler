module Euler.Problem118
( problem118
) where

import Euler.Util

import Control.Arrow
import Data.List (foldl', permutations)
import Math.NumberTheory.Primes.Testing

digitsToInt :: [Int] -> Int
digitsToInt = foldl' (\c x -> 10*c + x) 0

checkPermutations :: [[Int]] -> Int
checkPermutations = sum . map (cp 0 9)
    where cp :: Int -> Int -> [Int] -> Int
          cp _ _ [] = 1
          cp p l xs = sum [ cp next (l-n) rest | n <- [1..l]
                          , let (next,rest) = first digitsToInt $ splitAt n xs
                          , next > p
                          , isPrime $ toInteger next ]

problem118 = NoInput . show . checkPermutations $ permutations [1..9]
