module Euler.Problem013
( problem13
) where

import Euler.Util

problem13 = HasInput $ take 10 . show . sum . map read . lines
