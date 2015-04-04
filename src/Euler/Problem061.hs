module Euler.Problem061
( problem61
) where

import Data.Maybe
import Euler.Util

triangles = takeWhile (<10000) . dropWhile (<=1000) $ scanl1 (+) [1..]
squares   = takeWhile (<10000) . dropWhile (<=1000) $ scanl1 (+) [1,3..]
pentagons = takeWhile (<10000) . dropWhile (<=1000) $ scanl1 (+) [1,4..]
hexagons  = takeWhile (<10000) . dropWhile (<=1000) $ scanl1 (+) [1,5..]
heptagons = takeWhile (<10000) . dropWhile (<=1000) $ scanl1 (+) [1,6..]
octagons  = takeWhile (<10000) . dropWhile (<=1000) $ scanl1 (+) [1,7..]

nums = map (\x -> (x,'T')) triangles ++ map (\x -> (x,'S')) squares ++ map (\x -> (x,'P')) pentagons ++ map (\x -> (x,'X')) hexagons ++ map (\x -> (x,'H')) heptagons ++ map (\x -> (x,'O')) octagons

magic chain used
    | length chain == 6 = if cycles (last chain) (head chain)
                          then Just chain
                          else Nothing
    | null left = Nothing
    | otherwise = let next = filter isJust $ map (\(a,b) -> magic (a:chain) (b:used)) left
                  in if null next
                     then Nothing
                     else head next
    where cycles x1 x2 = x1 `mod` 100 == x2 `div` 100
          left = filter ((`cycles` (head chain)) . fst) $
                 filter (not . (`elem` used) . snd) nums


problem61 = NoInput . show . sum . fromJust . head . filter isJust $ map (\x -> magic [x] "O") octagons
