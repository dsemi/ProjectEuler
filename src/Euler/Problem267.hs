module Euler.Problem267
( problem267
) where

import Euler.Util

-- minimize
func f = (9 * log 10 - 1000 * log (1-f))/( log (1 + 2*f) - log (1-f))

-- Hopefully 4 decimal places is close enough
findMin f stop start = fst $ fm start (0.0001, f 0.0001)
    where fm c (m,m')
              | c >= stop = if f c < m' then (c, f c) else (m,m')
              | otherwise = fm (c+0.0001) (if f c < m' then (c, f c) else (m,m'))

n `choose` r = product [n-r+1..n] `div` product [1..r]

round12 n = fromIntegral (round $ 10^12 * n) / (10^^12)

problem267 = let f       = findMin func 0.9999 0.0001
                 won  n  = n + 2 * f * n
                 lost n  = n - f * n
                 final r = (!! r) $ iterate won $ (!! (1000-r)) $ iterate lost 1
                 lim     = fromIntegral $ head [ r | r <- [1..1000], final r > 10^9]
             in NoInput . show . round12
                    $ sum (map (fromIntegral . choose 1000) [lim..1000]) / fromIntegral (2^1000)
