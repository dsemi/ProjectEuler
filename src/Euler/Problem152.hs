module Euler.Problem152
( problem152
) where

import Data.Ratio
import Euler.Util

partialSums n = let squares = [ 1%(i^2) | i <- [2..n]
                              , 2^4*3^2*5*7*13 `mod` i == 0
                              , i `notElem` [26,65,78] ]
                in zip squares $ scanr1 (+) squares

p152 :: Ratio Integer -> [(Ratio Integer, Ratio Integer)] -> Int
p152 curr parts
    | curr == 0 = 1
    | curr < 0  = 0
    | otherwise = let picks = zip [1..] . map fst $ takeWhile ((<=0) . (curr-) . snd) parts
                  in sum $ map (\(i,x) -> p152 (curr-x) $ drop i parts) picks

problem152 = NoInput . show . p152 (1%2) $ partialSums 80
