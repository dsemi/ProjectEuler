module Euler.Problem151
( problem151
) where

import Euler.Util

p151  0  0  0  1 = 0
p151  0  0  1  0 = p151 0 0 0 1 + 1
p151  0  1  0  0 = p151 0 0 1 1 + 1
p151  1  0  0  0 = p151 0 1 1 1 + 1
p151 a2 a3 a4 a5 = (pickA2 + pickA3 + pickA4 + pickA5) / (a2 + a3 + a4 + a5)
    where pickA2 = if a2 > 0 
                   then a2 * p151 (a2-1) (a3+1) (a4+1) (a5+1)
                   else 0
          pickA3 = if a3 > 0 
                   then a3 * p151 a2 (a3-1) (a4+1) (a5+1)
                   else 0
          pickA4 = if a4 > 0 
                   then a4 * p151 a2 a3 (a4-1) (a5+1)
                   else 0
          pickA5 = if a5 > 0 
                   then a5 * p151 a2 a3 a4 (a5-1)
                   else 0

rnd d p = (*p) . fromIntegral $ round (d / p)

problem151 = NoInputS . show $ rnd (p151 1 1 1 1) 0.000001
