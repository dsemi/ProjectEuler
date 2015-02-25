module Euler.Problem144
( problem144
) where

-- 4x^2 + y^2 = 100
-- y = x + 4
-- 5x^2 + 8x + 16 = 100

-- y - y1 = m (x - x1)
-- y = mx + (y1-m*x1)

import Data.Ord (comparing)
import Data.List (maximumBy)
import Euler.Util

data Line = Line { slope :: Double
                 , yintercept :: Double
                 , point :: (Double, Double)
                 } deriving (Show, Eq)

findLine x1 y1 x2 y2 = let m = (y2 - y1)/(x2 - x1)
                       in Line m (y1 - m*x1) (x2,y2)

findReflectedLine (Line m2 _ (x2,y2)) = 
    let m1 = y2 / (4*x2)
        m' = (m1^2 * m2 + 2*m1 - m2) / (1 + 2*m1*m2 - m1^2)
        b' = y2 - m' * x2
        -- (4 + m'^2)*x^2 + 2*m'*b'*x + (b'^2 - 100)
        a = 4 + m'^2
        b = 2*m'*b'
        c = b'^2 - 100
        x' = maximumBy (comparing (abs . subtract x2)) [ (-b + sqrt (b^2 - 4*a*c)) / (2*a)
                                                       , (-b - sqrt (b^2 - 4*a*c)) / (2*a) ]
    in Line m' b' (x', m'*x'+b')

problem144 = let initialLine = findLine 0 10.1 1.4 $ -9.6
             in NoInputI . length . takeWhile (\(Line _ _ (x,y)) -> abs x > 0.01 || y < 0) 
                    $ iterate findReflectedLine initialLine
