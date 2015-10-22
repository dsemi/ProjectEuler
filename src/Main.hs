{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.DeepSeq
import Control.Monad
import Data.List.Split
import Data.Maybe
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

import Euler.Problem001
import Euler.Problem002
import Euler.Problem003
import Euler.Problem004
import Euler.Problem005
import Euler.Problem006
import Euler.Problem007
import Euler.Problem008
import Euler.Problem009
import Euler.Problem010

import Euler.Problem011
import Euler.Problem012
import Euler.Problem013
import Euler.Problem014
import Euler.Problem015
import Euler.Problem016
import Euler.Problem017
import Euler.Problem018
import Euler.Problem019
import Euler.Problem020

import Euler.Problem021
import Euler.Problem022
import Euler.Problem023
import Euler.Problem024
import Euler.Problem025
import Euler.Problem026
import Euler.Problem027
import Euler.Problem028
import Euler.Problem029
import Euler.Problem030

import Euler.Problem031
import Euler.Problem032
import Euler.Problem033
import Euler.Problem034
import Euler.Problem035
import Euler.Problem036
import Euler.Problem037
import Euler.Problem038
import Euler.Problem039
import Euler.Problem040

import Euler.Problem041
import Euler.Problem042
import Euler.Problem043
import Euler.Problem044
import Euler.Problem045
import Euler.Problem046
import Euler.Problem047
import Euler.Problem048
import Euler.Problem049
import Euler.Problem050

import Euler.Problem051
import Euler.Problem052
import Euler.Problem053
import Euler.Problem054
import Euler.Problem055
import Euler.Problem056
import Euler.Problem057
import Euler.Problem058
import Euler.Problem059
import Euler.Problem060

import Euler.Problem061
import Euler.Problem062
import Euler.Problem063
import Euler.Problem064
import Euler.Problem065
import Euler.Problem066
import Euler.Problem067
import Euler.Problem068
import Euler.Problem069
import Euler.Problem070

import Euler.Problem071
import Euler.Problem072
import Euler.Problem073
import Euler.Problem074
import Euler.Problem075
import Euler.Problem076
import Euler.Problem077
import Euler.Problem078
import Euler.Problem079
import Euler.Problem080

import Euler.Problem081
import Euler.Problem082
import Euler.Problem083
import Euler.Problem084
import Euler.Problem085
import Euler.Problem086
import Euler.Problem087
import Euler.Problem088
import Euler.Problem089
import Euler.Problem090

import Euler.Problem091
import Euler.Problem092
import Euler.Problem093
import Euler.Problem094
import Euler.Problem095
import Euler.Problem096
import Euler.Problem097
import Euler.Problem098
import Euler.Problem099
import Euler.Problem100

import Euler.Problem101
import Euler.Problem102
import Euler.Problem104
import Euler.Problem107
import Euler.Problem108
import Euler.Problem109

import Euler.Problem112
import Euler.Problem113
import Euler.Problem114
import Euler.Problem115
import Euler.Problem116
import Euler.Problem117
import Euler.Problem118
import Euler.Problem119
import Euler.Problem120

import Euler.Problem121
import Euler.Problem122
import Euler.Problem123
import Euler.Problem124
import Euler.Problem125
import Euler.Problem127
import Euler.Problem129
import Euler.Problem130

import Euler.Problem132
import Euler.Problem133
import Euler.Problem134
import Euler.Problem139

import Euler.Problem142
import Euler.Problem144
import Euler.Problem145
import Euler.Problem148

import Euler.Problem151
import Euler.Problem152
import Euler.Problem160

import Euler.Problem162
import Euler.Problem164

import Euler.Problem178

import Euler.Problem181
import Euler.Problem185
import Euler.Problem187
import Euler.Problem188

import Euler.Problem191

import Euler.Problem204
import Euler.Problem205
import Euler.Problem206
import Euler.Problem207

import Euler.Problem243

import Euler.Problem267

import Euler.Problem277

import Euler.Problem357

import Euler.Problem387

import Euler.Problem443

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

maybeRun :: Int -> IO Double
maybeRun n = maybe (printf notfound n >> return 0) (run n) $ lookup n problems
    where notfound = "Problem %d is not implemented\n"
          str      = "Problem %3d: %28s  Elapsed time %s%.3f%s seconds\n"
          resetCode = setSGRCode [Reset]
          colorCode n = setSGRCode [SetColor Foreground Dull c]
              where c | n < 0.5   = Green
                      | n < 1     = Yellow
                      | otherwise = Red
          run :: (Integral a) => Int -> Problem -> IO Double
          run n p = do
            start <- getCPUTime
            ans <- case p of
                     (NoInput prob)  -> return prob
                     (HasInput prob) -> prob <$> findInput n
                     (HasRando prob) -> prob <$> newStdGen
            rnf ans `seq` return ()
            end <- getCPUTime
            let elapsedTime = fromIntegral (end - start) / 10^12
            printf str n ans (colorCode elapsedTime) elapsedTime resetCode
            return elapsedTime

main = do
  basedir <- getProgPath -- Need to chdir to here
  args <- liftM parseArgs getArgs
  let ps = probs args
  hSetBuffering stdout LineBuffering
  totalTime <- foldM (\acc x -> (+acc) <$> maybeRun x) 0 ps
  printf "Total   %3d: %48.3f seconds\n" (length ps) totalTime

   -- TODO: find missing problems
   -- TODO: analyze (sort)
   -- TODO: could implement adding of new problem which adds the import/uncomments and dls problem description
