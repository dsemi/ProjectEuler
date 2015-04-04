module Euler.Problem015
( problem15
) where

import Euler.Util

problem15 = NoInput . show $ 40 `choose` 20
    where n `choose` r = product [n-r+1..n] `div` product [1..r]
