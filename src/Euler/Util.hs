module Euler.Util where

import Data.Semigroup
import System.Random


data Problem = NoInput String
             | HasInput (String -> String)
             | HasRando (StdGen -> String)

over3 :: (a -> b) -> (a, a, a) -> (b, b, b)
over3 f (a, b, c) = (f a, f b, f c)

data Matrix = Matrix { x00 :: !Integer, x01 :: !Integer
                     , x10 :: !Integer, x11 :: !Integer } deriving (Show)

instance Semigroup Matrix where
    Matrix l00 l01 l10 l11 <> Matrix r00 r01 r10 r11 =
        Matrix { x00 = l00 * r00 + l01 * r10, x01 = l00 * r01 + l01 * r11
               , x10 = l10 * r00 + l11 * r10, x11 = l10 * r01 + l11 * r11 }

instance Monoid Matrix where
    mempty = Matrix 1 0 0 1

fibonacci :: Integer -> Integer
fibonacci n = x01 $ mtimesDefault n matrix
    where matrix = Matrix 0 1 1 1
