module Euler.Util where

data Problem a = NoInput String
               | HasInput (String -> String)
               | HasRando (Int, Int) ([Int] -> String)

over3 :: (a -> b) -> (a, a, a) -> (b, b, b)
over3 f (a, b, c) = (f a, f b, f c)
