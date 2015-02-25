module Euler.Problem178
( problem178
) where

import Data.Ix (range)
import qualified Data.Map as M
import Euler.Util

data StepState a = StepState { minDigit  :: a
                             , maxDigit  :: a
                             , lastDigit :: a
                             } deriving (Show, Eq, Ord)


isPandigital (StepState i a _) = i == 0 && a == 9
neighborStates m s@(StepState i a n) = map (\x -> (x, M.findWithDefault 0 s m)) $
    [StepState (min i (n - 1)) a (n - 1), StepState i (max a (n + 1)) (n + 1)]

allStates    = [StepState i a n | (i, a) <- range ((0,0),(9,9)), n <- [i..a]]
initialState = M.fromDistinctAscList [(StepState i i i, 1) | i <- [1..9]]
stepState m  = M.fromListWith (+) $ allStates >>= neighborStates m
numSolutionsInMap    = sum . map snd . filter (isPandigital . fst) . M.toList
numSolutionsOfSize n = sum . map numSolutionsInMap . take n $ iterate stepState initialState
 
problem178 = NoInputI $ numSolutionsOfSize 40
