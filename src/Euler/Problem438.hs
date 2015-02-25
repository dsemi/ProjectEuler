module Euler.Problem438
( problem438
) where

import Data.List

shift n l = l ++ replicate n 0

pad n l = replicate n 0 ++ l
 
norm :: (Fractional a, Eq a) => [a] -> [a]
norm = dropWhile (== 0)
 
deg l = length (norm l) - 1
 
zipWith' op p q = zipWith op (pad (-d) p) (pad d q)
  where d = (length p) - (length q)

polydiv :: (Eq a, Fractional a) => [a] -> [a] -> ([a], [a])
polydiv f g = aux (norm f) (norm g) []
  where aux f s q | ddif < 0 = (q, f)
                  | otherwise = aux f' s q'
           where ddif = (deg f) - (deg s)
                 k = (head f) / (head s)
                 ks = map (* k) $ shift ddif s
                 q' = zipWith' (+) q $ shift ddif [k]
                 f' = norm $ tail $ zipWith' (-) f ks

deriv :: (Fractional a) => [a] -> [a]
deriv xs = fst . foldr (\x (a,b) -> (x*b:a,b+1)) ([], 1) $ init xs

nextSturm :: (Eq b, Fractional b) => [[b]] -> [[b]]
nextSturm xs = xs ++ [map negate $ snd . polydiv (last $ init xs) $ last xs]

sturmSequence' :: (Eq a, Fractional a) => [[a]] -> [[a]]
sturmSequence' start
    | null $ last start = init start
    | otherwise         = sturmSequence' $ nextSturm start

sturmSequence :: (Eq a, Fractional a) => [a] -> [[a]]
sturmSequence xs = sturmSequence' [xs,deriv xs]

f :: (Num c) => c -> [c] -> c
f x p = fst . foldr (\t (a,b) -> (a + t*x^b, b+1)) (0,0) $ p

numSignChanges :: (Eq b, Num b, Num c) => [b] -> c
numSignChanges xs = fst . foldl magic (0,0) $ filter (/=0) xs
    where
      magic (a,b) x = let s = signum x
                      in (if abs (s-b) == 2 then a+1 else a, s)

rootsOverInterval :: (Eq b, Fractional b, Num a) => [[b]] -> b -> b -> a
rootsOverInterval seq a b = numSignChanges (map (f a) seq) - numSignChanges (map (f b) seq)

passes :: (Eq c, Fractional c) => Int -> [c] -> Bool
passes n p = let edges = map (\x -> if f (fromIntegral x) p == 0 then 1 else 0) [1..n+1]
                 seq = sturmSequence p
             in all (\x -> x) (map (\a -> rootsOverInterval seq (fromIntegral a) (fromIntegral $ a+1) - (edges !! a) + (edges !! (a-1)) == 1) [1..n])

poly2 = print [[a,b] | a <- [-3,-4], b <- [2..6], passes 2 [1,a,b]]

poly3 = print [[a,b,c] | a <- [-6,-7..(-8)], b <- [11..25], c <- [-6,-7..(-23)], passes 3 [1,a,b,c]]

poly4 = print [[a,b,c,d] | a <- [-10,-11..(-13)], b <- [35..70], c <- [-50,-51..(-153)], d <- [24..119], passes 4 [1,a,b,c,d]]

poly5 = print [[a,b,c,d,e] | a <- [-15,-16..(-19)], b <- [85..154], c <- [-225,-226..(-579)], d <- [274..1043], e <- [-120,-121..(-719)], passes 5 [1,a,b,c,d,e]]

poly7 = print [[a,b,c,d,e,f,g] | a <- [-28,-29..(-34)], b <- [322..510], c <- [-1960,-1961..(-4024)], d <- [6769..18423], e <- [-13132,-13133..(-48859)], f <- [13068..69263], g <- [-5040,-5041..(-40319)], passes 7 [1,a,b,c,d,e,f,g]]

main = poly4


polyFromRoots2 x1 x2 = (a1,a2)
    where
      subs = subsequences [x1,x2]
      a1 = negate . sum . concat $ filter ((==1) . length) subs
      a2 = sum . map product $ filter ((==2) . length) subs

polyFromRoots3 x1 x2 x3 = (a1,a2,a3)
    where
      subs = subsequences [x1,x2,x3]
      a1 = negate . sum . concat $ filter ((==1) . length) subs
      a2 = sum . map product $ filter ((==2) . length) subs
      a3 = negate . sum . map product $ filter ((==3) . length) subs

polyFromRoots4 x1 x2 x3 x4 = (a1,a2,a3,a4)
    where
      subs = subsequences [x1,x2,x3,x4]
      a1 = negate . sum . concat $ filter ((==1) . length) subs
      a2 = sum . map product $ filter ((==2) . length) subs
      a3 = negate . sum . map product $ filter ((==3) . length) subs
      a4 = sum . map product $ filter ((==4) . length) subs

polyFromRoots5 x1 x2 x3 x4 x5 = (a1,a2,a3,a4,a5)
    where
      subs = subsequences [x1,x2,x3,x4,x5]
      a1 = negate . sum . concat $ filter ((==1) . length) subs
      a2 = sum . map product $ filter ((==2) . length) subs
      a3 = negate . sum . map product $ filter ((==3) . length) subs
      a4 = sum . map product $ filter ((==4) . length) subs
      a5 = negate . sum . map product $ filter ((==5) . length) subs

polyFromRoots6 x1 x2 x3 x4 x5 x6 = (a1,a2,a3,a4,a5,a6)
    where
      subs = subsequences [x1,x2,x3,x4,x5,x6]
      a1 = negate . sum . concat $ filter ((==1) . length) subs
      a2 = sum . map product $ filter ((==2) . length) subs
      a3 = negate . sum . map product $ filter ((==3) . length) subs
      a4 = sum . map product $ filter ((==4) . length) subs
      a5 = negate . sum . map product $ filter ((==5) . length) subs
      a6 = sum . map product $ filter ((==6) . length) subs

polyFromRoots7 x1 x2 x3 x4 x5 x6 x7 = (a1,a2,a3,a4,a5,a6,a7)
    where
      subs = subsequences [x1,x2,x3,x4,x5,x6,x7]
      a1 = negate . sum . concat $ filter ((==1) . length) subs
      a2 = sum . map product $ filter ((==2) . length) subs
      a3 = negate . sum . map product $ filter ((==3) . length) subs
      a4 = sum . map product $ filter ((==4) . length) subs
      a5 = negate . sum . map product $ filter ((==5) . length) subs
      a6 = sum . map product $ filter ((==6) . length) subs
      a7 = negate . sum . map product $ filter ((==7) . length) subs
