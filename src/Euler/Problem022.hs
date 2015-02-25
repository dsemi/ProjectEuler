module Euler.Problem022
( problem22
) where

import Data.List (sort,foldl')
import Data.String.Utils
import Data.Char (ord)
import Control.Monad
import Euler.Util

p22 nameFile = let names = sort . split "\",\"" . init . tail $ strip nameFile
               in foldl' nameVal 0 $ zip [1..] names
    where nameVal t (i,n) = t + i * sum (map ((subtract 64) . ord) n)

problem22 = HasInputI p22
