module Puzzle.TVTropes (tropes) where

import Puzzle.Web
import Data.Char
import Data.Maybe
import Data.List

-- | Parse TVTropes to get a list of tropes
tropes :: String -- ^ url of the source page of a TVTropes article
       -> IO [String] -- ^ list of (2+)-word tropes on that page
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