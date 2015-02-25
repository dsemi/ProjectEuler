module Euler.Problem096
( problem96
) where

import Text.Regex
import Data.Array
import Data.Maybe
import Control.Monad
import Data.List (delete,elem,foldl',nub)
import Euler.Util

type Digit = Char
type Square = (Char,Char)
type Unit = [Square]
type Grid = Array Square [Digit]

cross :: String -> String -> [Square]
cross as bs = [(a,b) | a <- as, b <- bs]

rows   = "ABCDEFGHI"
digits = "123456789"
cols   = digits
box = (('A','1'),('I','9'))

squares :: [Square]
squares = cross rows cols

unitlist :: [Unit]
unitlist = [cross rows [c] | c <- cols] ++
           [cross [r] cols | r <- rows] ++
           [cross rs cs | rs <- ["ABC","DEF","GHI"]
           , cs <- ["123","456","789"]]

units :: Array Square [Unit]
units = array box [(s, [filter (/=s) u | u <- unitlist, s `elem` u]) | s <- squares]

peers :: Array Square [Square]
peers = array box [(s, nub . concat $ units ! s) | s <- squares]

allPossibilities :: Grid
allPossibilities = array box [(s,digits) | s <- squares]

parseGrid :: String -> Maybe Grid
parseGrid grid = do
  regularGrid grid
  foldM assign allPossibilities $ zip squares grid
    where regularGrid :: String -> Maybe String
          regularGrid g = if all (`elem` "0.-123456789") g
                          then Just g
                          else Nothing

assign :: Grid -> (Square, Digit) -> Maybe Grid
assign g (s,d) = if d `elem` digits
                 then let ds = g ! s
                          toDump = delete d ds
                      in foldM eliminate g $ zip (repeat s) toDump
                 else return g

eliminate :: Grid -> (Square, Digit) -> Maybe Grid
eliminate g (s,d) = let cell = g ! s
                    in if d `notElem` cell
                       then return g
                       else do
                         let newCell = delete d cell
                             newV = g // [(s,newCell)]
                         newV2 <- case newCell of
                                    []   -> Nothing
                                    [d'] -> let peersOfS = peers ! s
                                            in foldM eliminate newV . zip peersOfS 
                                               $ repeat d'
                                    _    -> return newV
                         foldM (locate d) newV2 $ units ! s

locate :: Digit -> Grid -> Unit -> Maybe Grid
locate d g u = case filter ((d `elem`) . (g !)) u of
                 []  -> Nothing
                 [s] -> assign g (s,d)
                 _   -> return g

search :: Grid -> Maybe Grid
search g = 
    case [(l,(s,xs)) | (s,xs) <- assocs g, let l = length xs, l /= 1] of
      [] -> return g
      ls -> let (_,(s,ds)) = minimum ls
            in msum [assign g (s,d) >>= search | d <- ds]

solve :: String -> Maybe Grid
solve str = parseGrid str >>= search

problem96 = HasInputI $ foldl' (\s grid -> let (Just g) = solve grid
                                           in s + read ((g ! ('A','1')) ++
                                                        (g ! ('A','2')) ++
                                                        (g ! ('A','3')))
                               ) 0 . map (filter (/='\n')) . tail 
                                         . splitRegex (mkRegex "Grid [0-9][0-9]")
  
