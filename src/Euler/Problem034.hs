module Euler.Problem034
( problem34
) where

import Data.Maybe (fromJust)
import Data.List (foldl',lookup)
import Data.Map ((!),insertWith,empty,findWithDefault)
import Euler.Util

memoFact = (map fact [0..] !!)
    where fact 0 = 1
          fact n = n * memoFact (n-1)

sumFactDigs n = sfd n 0
    where sfd 0 c = c
          sfd n c = sfd (n `div` 10) (memoFact (n `mod` 10) + c)

check b [] = b
check (c,xs) (num:nums) = let f = sumFactDigs num
                          in check (if f == num then c+num else c,f:xs) nums


problem34 = let (count,sums)   = check (0,[]) [10000,9999..3]
                cache          = zip [1..] $ 1:2:sums
                reverseCache   = foldl' accumCache empty cache
                accumCount c i = let delta = i - fromJust (lookup (i `div` 10000) cache)
                                 in foldl' (\s n -> s+i+n) c $ findWithDefault [] (-delta) reverseCache
            in NoInput . show $ foldl' accumCount count [10000,20000..7 * memoFact 9]
    where accumCache m (k,v) = insertWith (++) d [k] m
              where e = if k < 1000 then 4 - length (show k) else 0
                    d = k - v - e

