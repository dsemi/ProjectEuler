module Euler.Problem066
( problem66
) where

import Control.Arrow
import Data.List.Ordered (minus)
import Euler.Util

sqrtContinuedFraction n = [a' | (_,_,a') <- iterate nextTriplet (m0,d0,a0)]
    where m0 = 0
          d0 = 1
          a0 = truncate . sqrt $ fromIntegral n
          nextTriplet (m,d,a) = let m' = d*a-m
                                    d' = (n-m'^2) `div` d
                                    a' = (a0+m') `div` d'
                                in (m', d', a')

getConvergents (a0:a1:as) = let pqs = (p0,q0):(p1,q1):zipWith3 nextConvergent pqs (tail pqs) as
                            in pqs
    where p0 = a0
          q0 = 1
          p1 = a1*a0 + 1
          q1 = a1
          nextConvergent (p,q) (p',q') a = (a*p' + p, a*q' + q)

getPellSolutions n = [(p,q) | (p,q) <- convergents, p^2 - n*q^2 == 1]
    where convergents = getConvergents $ sqrtContinuedFraction n

problem66 = NoInputI . snd . maximum . map ((head . getPellSolutions) &&& id) 
            $ [2..1000] `minus` scanl1 (+) [1,3..]
