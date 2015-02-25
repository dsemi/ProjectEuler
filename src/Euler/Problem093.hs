module Euler.Problem093
( problem93
) where

import Data.Char
import Data.Maybe
import Control.Arrow
import Control.Monad
import qualified Data.IntSet as S
import Data.List (tails, permutations)
import Euler.Util

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                    , ys <- combinations (n-1) xs']

plus :: Int -> Int -> Maybe Int
a `plus` b = Just $ a+b

minus :: Int -> Int -> Maybe Int
a `minus` b = Just $ a-b

mult :: Int -> Int -> Maybe Int
a `mult` b = Just $ a*b

divide :: Int -> Int -> Maybe Int
a `divide` b
    | a `mod` b == 0 = Just $ a `div` b
    | otherwise      = Nothing

operations = combinations 3 $ concatMap (replicate 3) [plus,minus,mult,divide]

apply :: [Int] -> [Int]
apply nums = let xs = [ S.fromList $ catMaybes [o1, o2, o3, o4, o5] | [a,b,c,d] <- permutations nums
                      , [p,q,r] <- operations
                      , let o1 = p a b >>= \x -> q x c >>= \y -> r y d
                      , let o2 = join $ liftM2 q (p a b) (r c d)
                      , let o3 = q b c >>= p a >>= \x -> r x d
                      , let o4 = q b c >>= \x -> r x d >>= p a
                      , let o5 = r c d >>= q b >>= p a ]
             in S.toAscList $ S.unions xs

numConsec :: [Int] -> Int
numConsec = length . takeWhile id . zipWith (==) [1..] . dropWhile (<=0)

problem93 = NoInputS . map intToDigit . snd . maximum 
            . map (numConsec . apply &&& id) $ combinations 4 [1..9]
