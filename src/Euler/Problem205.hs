module Euler.Problem205
( problem205
) where

import Control.Monad
import Data.List (sort)
import Euler.Util

genOutputs size num = map sum . sequence $ replicate num [1..size]

pss = genOutputs 4 9

css = sort $ genOutputs 6 6

problem205 = NoInput . show $ round7 $ fromIntegral (sum $ map (\p -> length $ takeWhile (<p) css) pss) / (4^9 * 6^6)
    where round7 = (/ 10^7) . fromIntegral . round . (* 10^7)
