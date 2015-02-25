module Euler.Problem011
( problem11
) where

import Data.List
import Euler.Util

maxProduct4 xs = maximum $ zipWith4 (\w x y z -> w*x*y*z) xs (tail xs) (drop 2 xs) (drop 3 xs)

maxProductRowCol4 g = maximum $ map maxProduct4 g ++ map maxProduct4 (transpose g)

maxProductDiag4 g = maximum $ map maxProduct4 (filter ((>=4) . length) (left1 ++ tail left2 ++ right1 ++ tail right2))
    where 
      fg     = map reverse g
      left1  = transpose $ zipWith drop [0..] g
      left2  = transpose $ zipWith drop [0..] (transpose g)
      right1 = transpose $ zipWith drop [0..] fg
      right2 = transpose $ zipWith drop [0..] (transpose fg)

maxGridProduct4 :: (Integral a) => [[a]] -> a
maxGridProduct4 g = max rowsCols diags
    where rowsCols = maxProductRowCol4 g
          diags    = maxProductDiag4 g

problem11 = HasInputI $ maxGridProduct4 . map ((map read) . words) . lines
