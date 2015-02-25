module Euler.Problem029
( problem29
) where

import qualified Data.Set as S
import Euler.Util

problem29 = NoInputI . S.size $ S.fromList [a^b | a <- [2..100], b <- [2..100]]
