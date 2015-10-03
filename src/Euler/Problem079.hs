module Euler.Problem079
( problem79
) where

import Data.Map (fromList,unions,(!),member)
import Data.List (nub,sortBy)
import Euler.Util

genGT xs = fromList . map (\a -> (a,GT)) $ _genGT xs
    where _genGT [x]    = []
          _genGT (x:xs) = _genGT xs ++ map (\a -> (a,x)) xs

cmp m a b = if member (a,b) m then m ! (a,b) else EQ

p79 keylog = let numStrings = lines keylog
                 m          = unions $ map genGT numStrings
                 x          = nub $ concat numStrings
             in sortBy (cmp m) x

problem79 = HasInput p79
