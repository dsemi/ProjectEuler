module Euler.Problem102
( problem102
) where

import Data.List.Split
import Euler.Util

data Point = Point Double Double deriving (Show)

area p1 p2 p3 = let a = distance p1 p2
                    b = distance p1 p3
                    c = distance p2 p3
                    s = (a+b+c)/2
                in sqrt (s*(s-a)*(s-b)*(s-c))

distance (Point x1 y1) (Point x2 y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

containsOrigin (p1,p2,p3) = sum (map ($ Point 0 0) [area p1 p2,area p1 p3,area p2 p3]) - area p1 p2 p3 < 0.0001

problem102 = HasInputI $ foldl (\a t -> if containsOrigin t then a+1 else a) 0 
             . map ((\[a1,a2,b1,b2,c1,c2] -> (Point a1 a2, Point b1 b2, Point c1 c2)) 
                   . map read . splitOn ",") . lines
  
