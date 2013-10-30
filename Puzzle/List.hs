module Puzzle.List where

chunks :: Int -> [a] -> [[a]]
chunks n = map (take n) . takeWhile (not . null) . iterate (drop n) 
