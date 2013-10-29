module Puzzle.Scrabble 
       (module Puzzle.Scrabble,
        module Control.Monad.Reader
       ) where
             
import Control.Monad.Reader
import Data.Maybe
import Data.Char
import Control.Applicative

type Scrabble = Reader Scorer
type Scorer = (Char -> Int)

scorerFromTable t c = fromJust $ lookup (toUpper c) st
  where
    st = zip ['A'..'Z'] t

scrabble :: Scorer
scrabble = scorerFromTable [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]

wwf :: Scorer
wwf = scorerFromTable  [1,4,4,2,1,4,3,3,1,10,5,2,4,2,1,4,10,1,1,1,2,5,4,8,3,10]

-- Usage: runReader (wordScore "HeLlO") scrabble
letterScores :: String -> Scrabble [Int]
letterScores s = asks (flip map s)

wordScore :: String -> Scrabble Int
wordScore = (sum <$>) . letterScores

