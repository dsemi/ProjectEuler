module Euler.Problems
( buildProbs
) where

import Data.List (sort)
import Language.Haskell.TH
import System.Path.Glob

buildProbs :: Q [Dec]
buildProbs = do
  pFiles <- runIO $ glob "src/Euler/Problem???.hs"
  let ps :: [Integer]
      ps = map (read . take 3 . drop 17) $ sort pFiles
  return [ FunD (mkName "problems")
           [ Clause [] (NormalB (ListE (map buildProb ps))) []
           ]
         ]
    where buildProb n = TupE [LitE (IntegerL n), VarE (mkName $ "problem" ++ show n)]
