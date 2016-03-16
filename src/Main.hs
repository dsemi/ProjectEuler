{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.DeepSeq
import Control.Monad
import Data.List.Split
import Data.Maybe
import Euler
import Euler.Util
import Euler.Problems
import Language.Haskell.TH
import System.Console.ANSI
import System.CPUTime
import System.Environment
import System.Environment.FindBin
import System.IO
import System.Random
import Text.Printf

data Arguments = Args { missing  :: Bool
                      , analyze  :: Bool
                      , help     :: Bool
                      , noans    :: Bool
                      , probs :: [Int]
                      } deriving (Show)

addMissing   (Args _ b c d e) = Args True b c d e
addAnalyze   (Args a _ c d e) = Args a True c d e
addHelp      (Args a b _ d e) = Args a b True d e
addNoans     (Args a b c _ e) = Args a b c True e
addProblem n (Args a b c d e) = Args a b c d $ n:e

parseArgs args = let (Args a b c d e) = foldr pa (Args False False False False []) args
                 in Args a b c d $ if null e then [1..500] else e
    where pa "missing" m = addMissing m
          pa "analyze" m = addAnalyze m
          pa a m
              | a == "-h" || a == "--help"    = addHelp m
              | a == "-n" || a == "--noans"   = addNoans m
              | all (`elem` '-':['0'..'9']) a = case map read (splitOn "-" a) of
                                                  [s,e] -> foldr addProblem m [s..e]
                                                  [n]   -> addProblem n m
                                                  _     -> undefined -- lazy
              | otherwise                     = undefined -- again

findInput :: Int -> IO String
findInput n = readFile $ "src/Euler/problem" ++ show n ++ ".txt"

$(buildProbs)

colorizeTime :: Double -> String
colorizeTime n = printf "%s%.3f%s" startCode n endCode
    where startCode = setSGRCode [SetColor Foreground Dull c]
          endCode   = setSGRCode [Reset]
          c | n < 0.5   = Green
            | n < 1     = Yellow
            | otherwise = Red

timeFunc :: (NFData a) => IO a -> IO (a, Double)
timeFunc f = do
  start <- getCPUTime
  result <- f
  rnf result `seq` return ()
  end <- getCPUTime
  let elapsedTime = fromIntegral (end - start) / 10^12
  return (result, elapsedTime)

maybeRun :: Int -> IO Double
maybeRun n = maybe notfound run $ lookup n problems
    where notfound = do
            printf "Problem %d is not implemented\n" n
            return 0
          str      = "Problem %3d: %28s  Elapsed time %s seconds\n"
          run :: (Integral a) => Problem -> IO Double
          run p = do
            (ans, elapsedTime) <- timeFunc $ case p of
                                               NoInput prob  -> return prob
                                               HasInput prob -> prob <$> findInput n
                                               HasRando prob -> prob <$> newStdGen
            printf str n ans $ colorizeTime elapsedTime
            return elapsedTime

main = do
  basedir <- getProgPath -- Need to chdir to here
  args <- liftM parseArgs getArgs
  let ps = probs args
  totalTime <- foldM (\acc -> liftM (+acc) . maybeRun) 0 ps
  printf "Total   %3d: %48.3f seconds\n" (length ps) totalTime

   -- TODO: find missing problems
   -- TODO: analyze (sort)
   -- TODO: could implement adding of new problem which adds the import/uncomments and dls problem description
