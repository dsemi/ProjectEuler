module Euler.Problem098
( problem98
) where

import Data.Maybe
import Control.Monad
import qualified Data.Map as M
import Data.List (tails, nub, sort)
import Euler.Util
import Math.NumberTheory.Powers

combos 0 _  = [ [] ]
combos n xs = [ y:ys | y:xs' <- tails xs
              , ys <- combos (n-1) xs']


sameKeySameVal [] = True
sameKeySameVal ((a,b):xs) = all (\(a',b') -> case () of 
                                               _ | a' == a -> b' == b
                                                 | b' == b -> a' == a
                                                 | otherwise -> True) xs && 
                            sameKeySameVal xs


sqGrams xs@(x:_) = let nd = length x
                       squares = map show . takeWhile (<10^nd) . dropWhile (<10^(nd-1)) 
                                 $ map (^2) [1..]
                   in maximum' [ maximum nums | p <- squares
                               , let pairs = zip x p
                               , sameKeySameVal pairs
                               , let cm = M.fromList pairs
                               , let nums = map (wordToNum cm) xs
                               , all isSquare nums
                               , all (/='0') (map ((cm M.!) . head) xs)]
    where wordToNum m = read . map (m M.!)
          maximum' [] = Nothing
          maximum' (x:_) = Just x

p98 wFile = let words = read ('[' : wFile ++ "]") :: [String]
            in maximum . mapMaybe sqGrams . filter ((>1) . length) . M.elems 
                   $ M.fromListWith (++) [(sort w,[w]) | w <- words]

problem98 = HasInputI p98
