module Euler.Problem054
( problem54
) where

import Euler.Util

import Control.Arrow
import Data.Ord
import Data.List (sortBy,group,splitAt)
import qualified Data.Map as M

nums  = reverse $ 'A':'K':'Q':'J':'T':['9','8'..'2']
suits = "CDHS"
cards = zipWith (\s cs -> map (\c -> [c,s]) cs) suits $ replicate 4 nums
cmp = M.fromList $ concatMap (`zip` [2..]) cards

handValue xs = (rank,tiebreak)
    where rank
             | flush && straight   = 9
             | hasKinds 4          = 8
             | all hasKinds [2,3]  = 7
             | flush               = 6
             | straight            = 5
             | hasKinds 3          = 4
             | 1 < length (kind 2) = 3
             | hasKinds 2          = 2
             | otherwise           = 1
          hand     = sortBy (\a b -> compare (cmp M.! b) (cmp M.! a)) xs
          vals     = map (cmp M.!) hand
          counts   = M.fromList . map (head &&& length) $ group vals
          flush    = let s = last (head hand) in all (\x -> last x == s) $ tail hand
          straight = all (==1) . zipWith (flip subtract) vals $ tail vals
          hasKinds = not . null . kind
          kind n   = sortBy (flip compare) $ M.keys $ M.filter (==n) counts
          tiebreak = [4,3,2,1] >>= kind

problem54 = HasInput $ show . length . filter (==GT)
            . map (uncurry (comparing handValue) . splitAt 5 . words) . lines
