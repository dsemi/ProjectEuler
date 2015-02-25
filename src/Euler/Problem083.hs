module Euler.Problem083
( problem83
) where

import Data.Ord
import Control.Monad
import Data.Array
import Data.Ix
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.List (minimumBy, foldl')
import Euler.Util

arrayMatrix lists = let rlen = length lists
                        clen = length $ head lists
                        r    = ((1,1), (rlen, clen))
                    in array r . zip (range r) $ concat lists

a_star start goal h m = search S.empty (S.singleton start) 
                        (M.singleton start (m ! start)) 
                        $ M.singleton start (m ! start + h ! start)
    where neighbors (r,c) = filter (inRange $ bounds m) [ (r-1,c), (r,c+1), (r+1,c) , (r,c-1)]
          search :: S.HashSet (Int,Int) -> S.HashSet (Int,Int) 
                 -> M.HashMap (Int,Int) Int -> M.HashMap (Int,Int) Int -> Int
          search closed open gs fs
              | S.null open     = 0
              | current == goal = gs M.! goal
              | otherwise       = let open'   = S.delete current open
                                      closed' = S.insert current closed
                                      neighbs :: [((Int,Int),Int)]
                                      neighbs = [ (n, ts) | n <- neighbors current
                                                , not (S.member n closed)
                                                , let ts = gs M.! current + m ! n
                                                , not (S.member n open') || ts < (gs M.! n) ]
                                      (op',gs',fs') = foldl' (\(o,ng,nf) (n,ts) -> (S.insert n o, M.insert n ts ng, M.insert n (ts + h ! n) nf)) (open',gs,fs) neighbs
                                  in search closed' op' gs' fs'
              where current = minimumBy (comparing (fs M.!)) $ S.toList open

p83 mFile = let matrix    = arrayMatrix . map (read . ('[':) . (++"]")) $ lines mFile
                bds       = bounds matrix
                ulim      = snd bds
                heuristic = let m = minimum $ elems matrix
                            in listArray bds . map (\(r,c) -> (uncurry (+) ulim)-r-c) $ range bds
            in a_star (1,1) ulim heuristic matrix

problem83 = HasInputI p83
