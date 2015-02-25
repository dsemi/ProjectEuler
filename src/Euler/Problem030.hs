module Euler.Problem030
( problem30
) where

import Data.Char (digitToInt)
import Euler.Util

problem30 = NoInputI $ sum [x | x <- [2..6*9^5], x == (sum . map ((^5) . digitToInt) . show) x]
