module Euler.Problem099
( problem99
) where

import Data.List.Split
import Euler.Util

problem99 = HasInputI $ snd . maximum . flip zip [1..] . map ((\[a,b] -> log a * b) . map read . splitOn ",") . lines
