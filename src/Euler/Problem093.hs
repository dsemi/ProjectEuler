module Euler.Problem093
( problem93
) where

import Control.Arrow
import Control.Monad
import Data.Char
import qualified Data.IntSet as S
import Data.List (tails, permutations)
import Data.Maybe
import Data.Ratio
import Euler.Util

data Operation = Plus | Minus | Multiply | Divide deriving (Show, Enum)
data Expr = Const (Ratio Int) | Apply Operation Expr Expr

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                    , ys <- combinations (n-1) xs' ]

eval :: Expr -> Maybe (Ratio Int)
eval (Const n)                  = Just n
eval (Apply Plus     exp1 exp2) = (+) <$> eval exp1 <*> eval exp2
eval (Apply Minus    exp1 exp2) = (-) <$> eval exp1 <*> eval exp2
eval (Apply Multiply exp1 exp2) = (*) <$> eval exp1 <*> eval exp2
eval (Apply Divide   exp1 exp2) = join $ divide <$> eval exp1 <*> eval exp2
    where divide a b
              | b /= 0    = Just $ a / b
              | otherwise = Nothing

numConsec :: [Int] -> Int
numConsec = length . takeWhile id . zipWith (==) [1..]
            . dropWhile (< 1) . S.toAscList . S.fromList . f
    where f nums = catMaybes [ numerator <$> res | ns <- permutations $ map (% 1) nums
                             , let [a, b, c, d] = map Const ns
                             , let ops = [ Plus .. ]
                             , i <- [0..63]
                             , let (op1, op2, op3) = ( ops !! (i `quot` 16 `rem` 4)
                                                     , ops !! (i `quot` 4 `rem` 4)
                                                     , ops !! (i `rem` 4) )
                             , tree <- [ Apply op3 (Apply op2 (Apply op1 a b) c) d
                                       , Apply op2 (Apply op1 a b) $ Apply op3 c d
                                       , Apply op3 (Apply op1 a $ Apply op2 b c) d
                                       , Apply op1 a $ Apply op3 (Apply op2 b c) d
                                       , Apply op1 a . Apply op2 b $ Apply op3 c d ]
                             , let res = eval tree
                             , liftM denominator res == Just 1 ]

problem93 = NoInput . map intToDigit . snd . maximum
            . map (numConsec &&& id) $ combinations 4 [1..9]
