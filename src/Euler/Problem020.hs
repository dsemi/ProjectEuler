module Euler.Problem020
( problem20
) where

import Data.Char (digitToInt)
import Euler.Util

problem20 = NoInputI . sumDigits $ f 100
    where sumDigits n = sum . map digitToInt $ show n
          f 0 = 1
          f n = n * f (n-1)
