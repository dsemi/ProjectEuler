module Euler.Problem084
( problem84
) where

import Data.Ord
import Control.Monad
import Data.List (sortBy)
import qualified Data.Map as M
import Euler.Util

us = [12,28]
rrs = [5,15,25,35]

inc2 xs n = head . dropWhile (`notElem` xs) $ iterate ((`mod` 40) . (+1)) n

cc = [id, id, id, id, id, id, id, id, id, id, id, id, id, id, const 0, const 10]
ch = [id, id, id, id, id, id, const 0, const 10, const 11, const 24, const 39, const 5, subtract 3, inc2 rrs, inc2 rrs, inc2 us]
g2j = [const 10]
other = [id]

squares = M.fromList $ zip [0..39] [ other, other, cc, other, other, other
                                   , other, ch, other, other, other, other
                                   , other, other, other, other, other, cc
                                   , other, other, other, other, ch, other
                                   , other, other, other, other, other, other
                                   , g2j, other, other, cc, other, other, ch
                                   , other, other, other ]

simulate :: Integer -> Int -> [(Int,Int)] -> [(Int,Int)]
simulate cnt sq rolls = M.toList $ sim cnt (M.fromList . zip [0..39] $ repeat 0) 0 0 squares rolls
    where sim c m s dc sqs (r:rs)
              | c == 0    = m
              | otherwise = if dc' == 3
                            then sim (c-1) (M.adjust (+1) 10 m)  10  0    sqs                          rs
                            else sim (c-1) (M.adjust (+1) ns' m) ns' dc' (M.insert ns (fs ++ [f]) sqs) rs
              where dc'    = if uncurry (==) r then dc+1 else 0
                    ns     = (s + uncurry (+) r) `mod` 40
                    (f:fs) = sqs M.! ns
                    ns'    = f ns

pad x = if length x == 1 then '0':x else x

p84 rs = concatMap (pad . show . fst) . take 3 . sortBy (flip $ comparing snd) . simulate (10^5) 0 
         $ zip rs (tail rs)

problem84 = HasRandoS (1,4) p84
