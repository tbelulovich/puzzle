module Puzzle.Web where

import Network.Curl
import Text.HTML.TagSoup
import Data.Maybe
import Text.HTML.TagSoup.Tree

getText :: String -> IO String
getText url = do (_,s) <- curlGetString url []
                 return s
                 
getTags :: String -> IO [Tag String]
getTags url = do t <- getText url
                 return $ parseTags t

stripTags :: String -> IO [String]
stripTags url = do t <- getTags url
                   return $ mapMaybe maybeTagText t
                
getTagTree :: String -> IO [TagTree String]
getTagTree = (fmap tagTree) . getTags

type TT = TagTree String 
type TreeParser a = TT -> [(a,TT)]
