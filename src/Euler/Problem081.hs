module Euler.Problem081
( problem81
) where

import Control.Monad
import Data.List.Split
import Data.List (foldl', foldl1', transpose)
import Euler.Util

diag1 fg = reverse . transpose . zipWith drop [0..] . transpose $ map reverse fg
diag2 fg = tail . transpose . zipWith drop [0..] $ map reverse fg

p81 mFile = let matrix = map (map read . splitOn ",") $ lines mFile
            in head . foldl' reduce2 (foldl1' reduce1 $ diag1 matrix) $ diag2 matrix
      where reduce1 a b = let (a':as) = zipWith (+) a b
                              bs      = zipWith (+) a (tail b)
                          in [a'] ++ zipWith min as bs ++ [last bs]
            reduce2 a b = zipWith (+) b . zipWith min a $ tail a

problem81 = HasInput $ show . p81
