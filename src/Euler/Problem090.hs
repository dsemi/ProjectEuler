module Euler.Problem090
( problem90
) where

import Control.Applicative
import Data.List (tails)
import Data.Set (member, fromList)
import Euler.Util

combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                    , ys <- combinations (n-1) xs' ]

repsAllNums ns ls ls' = all (reps ls ls') ns
    where reps xs ys [n1,n2] = member n1 xs && member n2 ys ||
                               member n2 xs && member n1 ys

problem90 = let choices = "0123456786"
                -- Replaced 9s w/ 6s because they are equivalent
                nums    = ["01", "04", "06", "16", "25", "36", "46", "64", "81"]
                dice    = map fromList $ combinations 6 choices
            in NoInput . show . (`div` 2) . length . filter id
                   $ repsAllNums nums <$> dice <*> dice
