module Euler.Problem019
( problem19
) where

import Data.List (foldl')
import qualified Data.Map as M
import Euler.Util

months = ["jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"]
monthSundays = M.fromList $ zip months [2, 5, 5, 1, 3, 6, 1, 4, 0, 2, 5, 0]

problem19 = let sundayCount = 2
            in NoInput . show . fst $ foldl' countSundays (sundayCount,monthSundays) [1902..2000]
    where countSundays (c,m) i = let m' = foldl' dayShift m months
                                     c' = M.foldl (\x v -> if v == 0 then x+1 else x) c m'
                                 in (c',m')
              where dayShift m month = let i' = if month == "jan" || month == "feb" then i-1 else i
                                           s  = if i' `mod` 4 == 0 then 2 else 1
                                           p  = m M.! month
                                       in M.insert month ((p+s) `mod` 7) m
