{-# LANGUAGE ExistentialQuantification #-}

module Euler.Util
where

data Problem a = NoInputS String 
               | HasInputS (String -> String)
               | HasRandoS (Int, Int) ([Int] -> String)
               | forall a. (Integral a) => NoInputI a 
               | forall a. (Integral a) => HasInputI (String -> a)
