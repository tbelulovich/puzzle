module Puzzle.Scrabble 
       (module Puzzle.Scrabble
       ) where
             
import Control.Monad.Reader
import Data.Maybe
import Data.Char
import Control.Applicative

type Scrabble = Reader Scorer
type Scorer = (Char -> Int)

runScrabble :: Scrabble a -> Scorer -> a
runScrabble = runReader

getScorer :: Scrabble Scorer
getScorer = ask

scorerFromTable :: [Int] -> Char -> Int
scorerFromTable t c = fromJust $ lookup (toUpper c) st
  where
    st = zip ['A'..'Z'] t

scrabble :: Scorer
scrabble = scorerFromTable [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]

wwf :: Scorer
wwf = scorerFromTable  [1,4,4,2,1,4,3,3,1,10,5,2,4,2,1,4,10,1,1,1,2,5,4,8,3,10]

phone :: Scorer 
phone = scorerFromTable phoneTable
  where phoneTable = [2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,7,8,8,8,9,9,9,9]

-- Usage: runReader (wordScore "HeLlO") scrabble
letterScores :: String -> Scrabble [Int]
letterScores s = asks (flip map s)

wordScore :: String -> Scrabble Int
wordScore = (sum <$>) . letterScores

