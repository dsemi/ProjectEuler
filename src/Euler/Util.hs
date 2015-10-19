module Euler.Util where

import System.Random

data Problem = NoInput String
             | HasInput (String -> String)
             | HasRando (StdGen -> String)

over3 :: (a -> b) -> (a, a, a) -> (b, b, b)
over3 f (a, b, c) = (f a, f b, f c)
