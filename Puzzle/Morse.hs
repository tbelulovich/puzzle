module Puzzle.Morse where


import Control.Applicative hiding (many)
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Text.ParserCombinators.ReadP

import Puzzle.Dict

morseTable :: [(Char, String)]
morseTable = 
  zipWith (,) 
  ['A'..'Z']
  [".-", "-...", "-.-.", "-..", ".", "..-.", "--."
  ,"....","..",".---","-.-",".-..","--","-.","---"
  ,".--.","--.-",".-.","...","-","..-","...-",".--"
  ,"-..-","-.--","--.."]

toMorse :: String -> String
toMorse = intercalate " "
          . mapMaybe (flip lookup morseTable)

morseParsers :: [ReadP Char]
morseParsers = map build morseTable where
  build (x,enc) =
    do string enc
       return x
       
morseLetter :: ReadP Char
morseLetter = choice morseParsers

-- Perform p until p fails
eager :: ReadP a -> ReadP [a]
eager p = do x <- many p
             fails p
             return x
             
fails :: ReadP a -> ReadP ()
fails p = do x <- (many1 p <++ return [])
             guard (null x)
             
mem x d =
  (x `elem` ["A","I","AM","ON","IN","OF","SO","IT"] || (length x >= 3))
  && S.member x d
             
morseBlobDict d =
  do x <- many1 morseLetter
     guard $ mem x d
     return x

morseWord = manyTill h eof where
  h = do x <- morseLetter
         eof +++ (char ' ' >> return ())
         skipSpaces
         return x
         
morseDictWord d =
  do x <- sepBy1 morseLetter (char ' ')
     guard (S.member x d)
     return x 

testBlob s = 
  do d <- stdDict
     let p = manyTill (morseBlobDict d) eof
     return $ readP_to_S p s

readInt :: String -> [(Int,String)]
readInt s = [(x,"")]
  where x = read s

wrd =  many1 $ satisfy isAlpha

testo = do
  x <- wrd
  fails wrd
  return x
