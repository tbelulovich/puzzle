module Puzzle.List
       ( chunks
       , subseq
       , histogram )
       where

import Data.List
import Control.Arrow

chunks :: Int -> [a] -> [[a]]
chunks n = map (take n) . takeWhile (not . null) . iterate (drop n) 

subseq [] _ = True
subseq _ [] = False
subseq (a:as) (b:bs) | a == b    = subseq as bs
                     | otherwise = subseq (a:as) bs

histogram l = map (head &&& length) $ group $ sort l
