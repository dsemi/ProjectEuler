module Euler.Problem122
( problem122
) where

import Data.List (tails)
import qualified Data.Map as M
import Euler.Util

min' Nothing  b = b
min' (Just a) b = min a b

paths n m d l
    | null children || cutoff = m
    | otherwise     = let m' = foldl (\ma x -> M.insert x (min' (M.lookup x ma) d) ma) m children
                      in foldl (\ma x -> paths n ma (d+1) (x:l)) m' children
    where children  = filter (<=n) $ map ((head l)+) l
          cutoff    = all (`M.member` m) children && all (<d) (map (m M.!) children)

expoMap n = let twos  = takeWhile (<=n) $ iterate (*2) 1
                m     = M.fromList $ zip twos [0..]
                trees = init . tails . reverse $ twos
            in foldl (\ma xs -> paths n ma (length xs) xs) m trees

problem122 = let n = 200
                 m = expoMap 200
             in NoInput . show . sum $ map (m M.!) [1..n] 
