module Euler.Problem016
( problem16
) where

import Data.Char (digitToInt)
import Euler.Util

problem16 = NoInput . show $ sumDigits (2^1000)
    where sumDigits n = sum . map digitToInt $ show n
