module Euler.Problem185
( problem185
) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Either (either)
import Control.Monad (foldM)
import Data.String.Utils (split)
import Data.Char (digitToInt, intToDigit)
import Data.Array.Unboxed (assocs, listArray, (!), (//), UArray)
import Euler.Util

params = [ "5616185650518293 ;2"
         , "3847439647293047 ;1"
         , "5855462940810587 ;3"
         , "9742855507068353 ;3"
         , "4296849643607543 ;3"
         , "3174248439465858 ;1"
         , "4513559094146117 ;2"
         , "7890971548908067 ;3"
         , "8157356344118483 ;1"
         , "2615250744386899 ;2"
         , "8690095851526254 ;3"
         , "6375711915077050 ;1"
         , "6913859173121360 ;1"
         , "6442889055042768 ;2"
         , "2321386104303845 ;0"
         , "2326509471271448 ;2"
         , "5251583379644322 ;2"
         , "1748270476758276 ;3"
         , "4895722652190306 ;1"
         , "3041631117224635 ;3"
         , "1841236454324589 ;3"
         , "2659862637316867 ;2" ]
         
solve = head . foldM (\a (b,c) -> next [] a b c) initsol
  where initsol :: [Either Int (UArray Int Bool)]
        initsol = replicate 16 . Right . listArray (0,9) $ replicate 10 True

next acc (s:ss) n (x:xs) = case s of
                             Right ys | ys ! x -> (if n == 0 then []
                                                   else next (Left x : acc) ss (n-1) xs)
                                                  ++ next (Right (ys // [(x,False)]) : acc) ss n xs
                             Left i  | i == x  -> if n == 0 then [] else next (s : acc) ss (n-1) xs
                             _                 -> next (s:acc) ss n xs
next acc _ 0 _ = return $ reverse acc
next acc _ _ _ = []

problem185 = NoInputS . map parseSol . solve . sortBy (comparing fst) 
       $ map parseL params
    where parseL x = let [num,c] = split " ;" x 
                     in (digitToInt (head c), map digitToInt num)
          getDigit = intToDigit . head . map fst . filter snd . assocs
          parseSol = either intToDigit getDigit
