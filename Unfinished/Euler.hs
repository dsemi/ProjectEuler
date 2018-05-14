module Euler 
where

import Control.Monad
import Text.HTML.TagSoup
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as L

getProblemInfo n = do
  tags <- liftM (parseTags . L.unpack) $ simpleHttp url
  let problemInfo = renderTags . head $ sections (~== "<div class=problem_content>") tags
  return $ html problemInfo
      where url = "https://projecteuler.net/problem=" ++ n
            html x = unlines [ "<!DOCTYPE html><html lang=\"en\"><head>"
                             , "<meta charset=\"utf-8\"></head><body>"
                             , x
                             , "</body></html>"
                             ]
