module Euler.Problem121
( problem121
) where

import Data.Ratio
import Data.Array
import Euler.Util

blue  = id
red   = (1-)
discs = listArray (0,1) [red,blue]          

genCombos n = map (map (discs !)) . filter ((> n `div` 2) . sum) $ gc n
    where gc 0 = [[]]
          gc x = [ c : cs | c <- [0,1]
                 , cs <- gc (x-1)]

problem121 = let x = 15
                 r = sum . map (product . flip (zipWith ($)) blueProbs) $ genCombos x
             in NoInput . show . floor $ denominator r % numerator r
    where blueProbs = map (1%) [2..]
