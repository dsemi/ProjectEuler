module Euler.Problem059
( problem59
) where

import Control.Arrow ((&&&))
import Control.Monad
import Data.Bits
import Data.Char (ord,chr)
import Data.Function
import Data.List (group,sort,maximumBy,transpose)
import Data.List.Split
import Euler.Util

mode xs = fst . maximumBy (compare `on` snd) $ elemCount
    where elemCount = map (head &&& length) . group. sort $ xs

p59 cipher = let str     = map read $ splitOn "," cipher
                 lists   = transpose $ chunksOf 3 str
                 key     = map (chr . (`xor` ord ' ') . mode) lists
                 message = map chr . zipWith xor str . map ord $ cycle key
             in sum $ map ord message

problem59 = HasInput $ show . p59
