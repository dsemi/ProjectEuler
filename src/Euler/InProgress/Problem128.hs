module Euler.Problem128
( problem128
) where

import Control.Arrow
import Data.Array

layerSizes = listArray (1,5000) $ 1 : [ y | x <- [6,12..], y <- replicate x x]

