module Euler.Problem032
( problem32
) where

import Data.List (sort)
import Data.Set (fromList, toList)
import Euler.Util

pandigital a b = sort (show a ++ show b ++ show (a*b)) == "123456789"

problem32 = NoInput . show . sum . toList . fromList $ [ a*b | a <- [1..50]
                                                 , b <- [1..2000]
                                                 , pandigital a b]
