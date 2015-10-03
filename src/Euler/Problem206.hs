module Euler.Problem206
( problem206
) where

import Control.Monad
import Control.Applicative
import Data.List (foldl')
import Data.Maybe
import Euler.Util

-- Could generate this with Template Haskell?
f ['1',_,'2',_,'3',_,'4',_,'5',_,'6',_,'7',_,'8',_,'9',_,'0'] = True
f [_,'2',_,'3',_,'4',_,'5',_,'6',_,'7',_,'8',_,'9',_,'0'] = True
f ['2',_,'3',_,'4',_,'5',_,'6',_,'7',_,'8',_,'9',_,'0'] = True
f [_,'3',_,'4',_,'5',_,'6',_,'7',_,'8',_,'9',_,'0'] = True
f ['3',_,'4',_,'5',_,'6',_,'7',_,'8',_,'9',_,'0'] = True
f [_,'4',_,'5',_,'6',_,'7',_,'8',_,'9',_,'0'] = True
f ['4',_,'5',_,'6',_,'7',_,'8',_,'9',_,'0'] = True
f [_,'5',_,'6',_,'7',_,'8',_,'9',_,'0'] = True
f ['5',_,'6',_,'7',_,'8',_,'9',_,'0'] = True
f [_,'6',_,'7',_,'8',_,'9',_,'0'] = True
f ['6',_,'7',_,'8',_,'9',_,'0'] = True
f [_,'7',_,'8',_,'9',_,'0'] = True
f ['7',_,'8',_,'9',_,'0'] = True
f [_,'8',_,'9',_,'0'] = True
f ['8',_,'9',_,'0'] = True
f [_,'9',_,'0'] = True
f ['9',_,'0'] = True
f [_,'0'] = True
f "0" = True
f _ = False

lastN n = foldl' (const . drop 1) <*> drop n

head' []           = Nothing
head' (Nothing:xs) = head' xs
head' (x:_)        = x

p206 :: Integer -> Int -> Maybe Integer
p206 sol 10 = do
  let square = show $ sol^2
  guard $ length square == 19
  guard $ f square
  return sol
p206 sol l = head' $ map test [9,8..0]
    where test d = do
            let num    = d*10^l + sol
                square = lastN (l+1) . show $ num^2
            guard $ f square
            p206 num (l+1)

problem206 = let result = p206 0 0
             in NoInput . show $ fromMaybe 0 result
