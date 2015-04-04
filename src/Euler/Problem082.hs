module Euler.Problem082
( problem82
) where

import Control.Monad
import Data.List (transpose, foldl1')
import Euler.Util

problem82 = HasInput $ show . minimum . foldl1' pathSum . transpose . map (read . ('[':) . (++"]")) . lines
    where pathSum (u:us) ve@(v:vs) = 
              let l1     = (u + v) : zipWith3 (\x y z -> x + min y z) vs l1 us
                  vs'    = tail $ reverse ve
                  (l:ls) = reverse l1
                  l2     = l : zipWith3 (\x y z -> min x (y+z)) ls l2 vs'
              in reverse l2
