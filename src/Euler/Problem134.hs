module Euler.Problem134
( problem134
) where

import Euler.Util
import Math.NumberTheory.Primes
import qualified Data.HashMap.Strict as M

mods :: M.HashMap Integer (M.HashMap Integer Integer)
mods = let ns = [1,3,7,9]
       in M.fromList . zip ns $ map (M.fromList . flip zip [0..] . zipWith (((`mod` 10) .) . (*)) [0..9] . repeat) ns

digits n = dig n []
    where dig 0 ds = ds
          dig x ds = dig (x `div` 10) $ x `mod` 10 : ds

findSs :: [(Integer,Integer)] -> Integer -> Integer
findSs [] c           = c
findSs ((p1,p2):ps) c = findSs ps $ c + findS p2 (reverse $ digits p1) 0

findS :: Integer -> [Integer] -> Integer -> Integer
findS p2 p1 r = let rDigs            = reverse $ digits r
                    (match,notMatch) = span (uncurry (==)) $ zip p1 rDigs
                    count            = length match
                    digsLeft         = drop count p1
                in case digsLeft of
                     (d:rest) -> let m = p2 `mod` 10
                                     nextD = if null notMatch then d 
                                             else (d - snd (head notMatch)) `mod` 10
                                 in findS p2 p1 (r + p2 * 10^count * ((mods M.! m) M.! nextD))
                     _        -> r

problem134 = let n = 1000000
             in NoInput . show $ findSs (takeWhile ((<n) . fst) . drop 2 . zip primes $ tail primes) 0
