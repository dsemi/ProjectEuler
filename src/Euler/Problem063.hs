module Euler.Problem063
( problem63
) where

import Euler.Util

counter p c n
    | length (show (n^p)) == p = counter (p+1) (c+1) n
    | otherwise                = c

problem63 = NoInput . show $ 1 + sum (takeWhile (>0) . map (counter 1 0) $ [2..])
