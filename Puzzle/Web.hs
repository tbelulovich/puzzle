module Puzzle.Web where

import Network.Curl
import Text.HTML.TagSoup
import Data.Maybe
import Text.HTML.TagSoup.Tree

getUrl :: String -> IO String
getUrl url = do
  (_,s) <- curlGetString url []
  return s
                 
tagsFromUrl :: String -> IO [Tag String]
tagsFromUrl url = do
  t <- getUrl url
  return $ parseTags t

stripTags :: String -> IO [String]
stripTags url = do
  t <- tagsFromUrl url
  return $ mapMaybe maybeTagText t
                
treeFromUrl :: String -> IO [TagTree String]
treeFromUrl = (fmap tagTree) . tagsFromUrl

type TT = TagTree String 
type TreeParser a = TT -> [(a,TT)]
