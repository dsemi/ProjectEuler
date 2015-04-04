module Euler.Util
where

data Problem a = NoInput String 
               | HasInput (String -> String)
               | HasRando (Int, Int) ([Int] -> String)
