module Euler.Problem107
( problem107
) where

import Control.Monad
import Data.List.Split
import Data.Map (empty,insert,(!))
import Data.List (sort,foldl',delete,elem)
import Euler.Util

connectGraph _ [] _ _ count                      = count
connectGraph g notIG iG ((fce,sce):bEdges) count = connectGraph g (delete sce notIG) (sce:iG) (sort . filter (not . (`elem` iG) . snd) $ bEdges ++ g ! sce) (count + fce)

p107 gFile = let grid          = map (splitOn ",") $ lines gFile
                 n             = length grid
                 (graph,total) = let (gr,tl) = fst $ traverseRowCol grid
                                 in (gr, tl `div` 2)
             in total - connectGraph graph [1..n-1] [0] (sort $ graph ! 0) 0
      where traverseRowCol            = foldl' traverseRow ((empty,0),0)
            traverseRow ((m,t),i) row = let (ls,rt,_) = foldl' traverseCol ([],0,0) row 
                                        in ((insert i ls m, t+rt), i+1)
            traverseCol (l,t',j) col  = if col == "-"
                                        then (l, t', j+1)
                                        else let ic = read col
                                             in ((ic,j):l, t'+ic, j+1)

problem107 = HasInputI p107
