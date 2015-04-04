module Euler.Problem088
( problem88
) where

import Data.STRef
import Data.Array.Base
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Data.List (sort, delete)
import qualified Data.Set as S
import Euler.Util
import Math.NumberTheory.Primes

allFactorizations :: Int -> [Int]
allFactorizations n = map (\x -> length x + n - sum x) 
                      . takeWhile ((>1) . length) . concat 
                      . iterate getAllFacts . (:[]) $ expandPrimeFactor n
expandPrimeFactor :: Int -> [Int]
expandPrimeFactor = concatMap (\(a,b) -> replicate b $ fromIntegral a) 
                    . factorise . fromIntegral
getAllFacts :: [[Int]] -> [[Int]]
getAllFacts = S.toList . S.fromList . concatMap combine
combine :: [Int] -> [[Int]]
combine xs = map sort $ next xs
    where mult a b = a*b : (delete b $ delete a xs)
          next [] = []
          next (y:ys) = map (mult y) ys ++ next ys

productSumNumbers :: Int -> UArray Int Int
productSumNumbers x = runSTUArray $ do
                        a <- newArray (0,x) 0
                        c <- newSTRef 0
                        addNumsToArray a c [2..]
                        return a
    where addNumsToArray a c (n:ns) = do
            forM_ (allFactorizations n) $ \k -> do
              when (k > 1 && k <= x) $ do
                prev <- unsafeRead a k
                when (prev == 0) $ do
                  unsafeWrite a k n
                  modifySTRef c (+1)
            cnt <- readSTRef c
            when (cnt < x-1) $ addNumsToArray a c ns


problem88 = NoInput . show . S.foldl' (+) 0 . S.fromList . elems $ productSumNumbers 12000
