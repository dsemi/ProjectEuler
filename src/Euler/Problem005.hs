module Euler.Problem005
( problem5
) where

import Euler.Util

problem5 = NoInputI $ foldl1 lcm [1..20]
