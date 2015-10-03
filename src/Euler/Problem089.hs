module Euler.Problem089
( problem89
) where

import Data.Map (fromList,(!))
import Data.List (sortBy)
import Euler.Util

rm = fromList [('I',1), ('V',5), ('X',10), ('L',50), ('C',100), ('D',500), ('M',1000)]
nm = [(1000,"M"), (900,"CM"), (500,"D"), (400,"CD"), (100,"C"), (90,"XC"), (50,"L"), (40,"XL"), (10,"X"), (9,"IX"), (5,"V"), (4,"IV"), (1,"I")]

romanToNum s = let n = map (rm !) s
               in sum . filter (>0) . zipWith (\a b -> if a == b then a else a-b) n $ sortBy (flip compare) n

numToRoman i l n
    | i == length nm - 1 = concat . reverse $ replicate n 'I':l
    | d >= 0              = numToRoman i (snd c:l) d
    | otherwise          = numToRoman (i+1) l n
    where c = nm !! i
          d = n - fst c

p89 roman = let old = lines roman
                new = map (numToRoman 0 [] . romanToNum) old
            in length (concat old) - length (concat new)

problem89 = HasInput $ show . p89
