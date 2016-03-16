module Euler.Problem042
( problem42
) where

import Data.List.Split
import Data.Char (ord)
import Data.IntSet (fromList, member)
import Euler.Util

-- Must be capital letter
getCharCode l = ord l - 64

getWordValue n = sum $ map getCharCode n

p42 x = length . filter (`member` triangles) $ nums
    where wordlist   = splitOn "," $ filter (/='\"') x
          upperlimit = sum . replicate (maximum $ map length wordlist) $ getCharCode 'Z'
          triangles  = fromList . takeWhile (<= upperlimit) $ scanl1 (+) [1..]
          nums       = map getWordValue wordlist

problem42 = HasInput $ show . p42
