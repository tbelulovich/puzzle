module Puzzle.TVTropes (tropes) where

import Puzzle.Web
import Data.Char
import Data.Maybe
import Data.List

--- takes as input the url of the source page of a TVTropes article,
--- and outputs a list of two-word tropes on the page
tropes :: String -> IO [String]
tropes url = do t <- stripTags url
                let s = map wds t
                return $ mapMaybe trope s
                
split :: (a -> Bool) -> [a] -> [[a]]
split p [] = []
split p x@(a:b) | p a = (takeWhile p x) : split p (dropWhile p x)
                | otherwise = split p b
                              
wds = split isAlpha                              
isCamelCased = (> 1) . length . filter isUpper

trope = find isCamelCased 