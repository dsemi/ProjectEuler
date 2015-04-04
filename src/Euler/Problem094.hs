module Euler.Problem094
( problem94
) where

-- x^2 - 3y^2 = 1

import Euler.Util

sqrtContinuedFraction :: Integral t => t -> [t]
sqrtContinuedFraction n = [a' | (_,_,a') <- iterate nextTriplet (m0,d0,a0)]
    where m0 = 0
          d0 = 1
          a0 = truncate . sqrt $ fromIntegral n
          nextTriplet (m,d,a) = let m' = d*a-m
                                    d' = (n-m'^2) `div` d
                                    a' = (a0+m') `div` d'
                                in (m', d', a')

getConvergents :: Integral a => [a] -> [(a,a)]
getConvergents (a0:a1:as) = let pqs = (p0,q0):(p1,q1):zipWith3 nextConvergent pqs (tail pqs) as
                            in pqs
    where p0 = a0
          q0 = 1
          p1 = a1*a0 + 1
          q1 = a1
          nextConvergent (p,q) (p',q') a = (a*p' + p, a*q' + q)

getPellSolutions :: Integral t => t -> [(t,t)]
getPellSolutions n = [(p,q) | (p,q) <- convergents, p^2 - n*q^2 == 1]
    where convergents = getConvergents $ sqrtContinuedFraction n

problem94 = let xys = takeWhile ((<=10^9) . subtract 1 . (*2) . fst) $ getPellSolutions 3
            in NoInput . show $ sum [ a3+1 | (x,y) <- xys
                              , let a3 = 2*x-1
                              , let area3 = y*(x-2)
                              , a3 > 0 && a3 `mod` 3 == 0
                              , area3 > 0 && area3 `mod` 3 == 0 ] + 
                          sum [ a3-1 | (x,y) <- xys
                              , let a3 = 2*x+1
                              , let area3 = y*(x+2)
                              , a3 > 0 && a3 `mod` 3 == 0
                              , area3 > 0 && area3 `mod` 3 == 0 ]
