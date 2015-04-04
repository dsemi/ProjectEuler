module Euler.Problem056
( problem56
) where

import Data.Char (digitToInt)
import Euler.Util

digs = sum . map digitToInt . show

problem56 = NoInput . show $ maximum [digs (a^b) | a <- [1..99], b <- [1..99]]
