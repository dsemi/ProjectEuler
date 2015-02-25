module Euler.Problem101
( problem101
) where


-- use matrix to solve the equations

-- matrix will be nxn eqn degree n-1

import Control.Arrow
import Data.Ratio
import Data.List (inits, foldl', find)
import Euler.Util

us = map (id &&& u) [1..]
    where u n = 1 - n + n^2 - n^3 + n^4 - n^5 + n^6 - n^7 + n^8- n^9 + n^10

rref :: [[Ratio Integer]] -> [[Ratio Integer]]
rref m = f m 0 [0 .. rows - 1]
    where rows                      = length m
          cols                      = length $ head m
          f m _    []               = m
          f m lead (r : rs)
              | indices == Nothing  = m
              | otherwise           = f m' (lead' + 1) rs
              where indices         = find p l
                    p (col, row)    = m !! row !! col /= 0
                    l               = [ (col, row) | col <- [lead .. cols - 1]
                                      , row <- [r .. rows - 1]]
                    Just (lead', i) = indices
                    newRow          = map (/ m !! i !! lead') $ m !! i
                    m'              = zipWith g [0..] $
                                      replace r newRow $
                                      replace i (m !! r) m
                    g n row
                        | n == r    = row
                        | otherwise = zipWith h newRow row
                        where h     = subtract . (* row !! lead')

replace :: Int -> a -> [a] -> [a]
{- Replaces the element at the given index. -}
replace n e l = a ++ e : b
    where (a, _ : b) = splitAt n l

problem101 = NoInputI . sum $ map (numerator . findFIT . op . lastCol . rref . findBOP) pointGroups
    where coords      = take 10 us
          vals        = map snd us
          pointGroups = tail $ inits coords
          findBOP pts = let d = length pts - 1
                        in flip map pts $ \(x,y) -> map (x^) [d,d-1..0] ++ [y]
          lastCol ms  = [ last m | m <- ms ]
          op abcs     = let d = length abcs - 1
                        in \x -> sum . zipWith (*) abcs $ map (x^) [d,d-1..0]
          findFIT     = findNoEq . zip vals . flip map [1..]
              where findNoEq ((a,b):xs)
                        | a == b    = findNoEq xs
                        | otherwise = b
