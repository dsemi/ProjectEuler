module Euler.Problem162
( problem162
) where

import Data.Array
import Euler.Util

applyUntil n f x' = au n f x' [x']
    where au n f x rs
              | n == fx   = rs
              | otherwise = au n f (fx) $ fx : rs
              where fx = f x

hex :: Int -> String
hex = map ((hexMap !) . (`mod` 16)) . applyUntil 0 (`div` 16)
    where hexMap = listArray (0,15) $ ['0'..'9'] ++ ['A'..'F']

--                All n digit nums - No 0 or 1 or A + No {0,1} or {0,A} or {1,A} - No {0,1,A}
numHexLengthN n = 15*16^(n-1)      - 43*15^(n-1)    + 41*14^(n-1)                - 13^n

problem162 = NoInputS . hex . sum $ map numHexLengthN [3..16]
