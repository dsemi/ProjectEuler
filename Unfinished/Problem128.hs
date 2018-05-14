module Euler.Problem128
( problem128
) where

import Data.List (foldl')
import Debug.Trace
import Euler.Util
import Math.NumberTheory.Primes
import qualified Data.HashMap.Strict as M

ftrans = [(0,2), (-1, 1), (-1, -1), (0, -2), (1, -1), (1, 1)]
trans = tail ftrans ++ [head ftrans]

uncurry3 f (a,b,c) = f a b c

addLayer :: M.HashMap (Integer,Integer) Integer -> Integer -> [(Integer, Integer)] -> (M.HashMap (Integer,Integer) Integer, Integer, [(Integer,Integer)])
addLayer m c outer_layer = addL True m c [] outer_layer
    where addL first m c oL' [] = (m,c,oL')
          addL first m c oL' ((x,y):oL) = let adjs = takeWhile (not . flip M.member m)
                                                     $ dropWhile (flip M.member m)
                                                     [ coord | (x',y') <- if first
                                                                          then ftrans
                                                                          else trans
                                                     , let coord = (x+x', y+y') ]
                                              mappings = zip adjs [c,c+1..]
                                              c' = (+1) . snd $ last mappings
                                              m' = foldl' (\n (k,v) -> M.insert k v n) m mappings
                                          in addL False m' c' (oL' ++ adjs) oL

pd m xy@(x,y) = sum [ 1 | (x',y') <- trans
                    , let coord = (x+x', y+y')
                    , isPrime . abs $ m M.! xy - m M.! coord ]

find3Pds = f (M.singleton (0,0) 1) 2 [(0,0)]
    where f m c oL = let (m', c', oL') = addLayer m c oL
                         pd3s =  map (m M.!) $ filter ((== 3) . pd m') oL
                     in pd3s ++ f m' c' oL'

problem128 = NoInput . show $ find3Pds !! 1999
