module Euler.Problem071
( problem71
) where

import Data.Ratio
import Euler.Util

fareySequence a b
    | da + db <= 10^6 = fareySequence a' b
    | otherwise       = a
    where na = numerator a
          da = denominator a
          nb = numerator b
          db = denominator b
          a' = (na+nb) % (da + db)


problem71 = NoInputI . numerator $ fareySequence (0%1) (3%7)
