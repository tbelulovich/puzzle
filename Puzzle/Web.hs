module Puzzle.Web where

import Network.Curl
import Text.HTML.TagSoup
import Data.Maybe


getText :: String -> IO String
getText url = do (_,s) <- curlGetString url []
                 return s
                 
getTags :: String -> IO [Tag String]
getTags url = do t <- getText url
                 return $ parseTags t

stripTags :: String -> IO [String]
stripTags url = do t <- getTags url
                   return $ mapMaybe maybeTagText t