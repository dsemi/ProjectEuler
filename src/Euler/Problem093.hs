module Euler.Problem093
( problem93
) where

import Euler.Util

import Control.Arrow
import Control.Monad
import Data.Char
import qualified Data.IntSet as S
import Data.Ix
import Data.List (tails, permutations)
import Data.Maybe

data Operation = (:+:) | (:-:) | (:*:) | (:/:) deriving (Eq, Ord, Ix)

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                    , ys <- combinations (n-1) xs' ]

toF (:+:) = (+)
toF (:-:) = (-)
toF (:*:) = (*)
toF (:/:) = (/)

numConsecGT :: Int -> [Int] -> Int
numConsecGT n = length . takeWhile id . zipWith (==) [ n+1 ..]
                . S.toAscList . S.fromList . f
    where f nums = [ res | ns <- permutations nums
                   , let [a, b, c, d] = map fromIntegral ns
                   , (op1, op2, op3) <- map (over3 toF)
                                        $ range ( ((:+:), (:+:), (:+:))
                                                , ((:/:), (:/:), (:/:)))
                   , approxRes <- [ ((a `op1` b) `op2` c) `op3` d
                                  , (a `op1` b) `op2` (c `op3` d)
                                  , (a `op1` (b `op2` c)) `op3` d
                                  , a `op1` ((b `op2` c) `op3` d)
                                  , a `op1` (b `op2` (c `op3` d)) ]
                   , let res = round approxRes
                   , abs (fromIntegral res - approxRes) < 0.0001
                   , res > n ]

problem93 = NoInput . map intToDigit . snd . maximum
            . map (numConsecGT 0 &&& id) $ combinations 4 [1..9]
