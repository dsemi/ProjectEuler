module Euler.Problem040
( problem40
) where

import Data.Char (digitToInt)
import Euler.Util

problem40 = NoInput . show $ product [ digitToInt $ decimal !! (10^x-1) | x <- [0..6]]
    where decimal = concatMap show [1..]
