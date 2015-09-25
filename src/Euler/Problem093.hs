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
data Expr = Const Int | Apply Expr Operation Expr

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                    , ys <- combinations (n-1) xs' ]

eval :: Expr -> Double
eval (Const n)               = fromIntegral n
eval (Apply exp1 (:+:) exp2) = eval exp1 + eval exp2
eval (Apply exp1 (:-:) exp2) = eval exp1 - eval exp2
eval (Apply exp1 (:*:) exp2) = eval exp1 * eval exp2
eval (Apply exp1 (:/:) exp2) = eval exp1 / eval exp2

numConsecGT :: Int -> [Int] -> Int
numConsecGT n = length . takeWhile id . zipWith (==) [1..]
                . S.toAscList . S.fromList . f
    where f nums = [ res | ns <- permutations nums
                   , let [a, b, c, d] = map Const ns
                   , (op1, op2, op3) <- range ( ((:+:), (:+:), (:+:))
                                              , ((:/:), (:/:), (:/:)))
                   , tree <- [ Apply (Apply (Apply a op1 b) op2 c) op3 d
                             , Apply (Apply a op1 b) op2 (Apply c op3 d)
                             , Apply (Apply a op1 (Apply b op2 c)) op3 d
                             , Apply a op1 (Apply (Apply b op2 c) op3 d)
                             , Apply a op1 (Apply b op2 (Apply c op3 d)) ]
                   , let approxRes = eval tree
                   , let res = round approxRes
                   , abs (fromIntegral res - approxRes) < 0.0001
                   , res >= n ]

problem93 = NoInput . map intToDigit . snd . maximum
            . map (numConsecGT 1 &&& id) $ combinations 4 [1..9]
