module Puzzle.Morse
       (morseLetter
       ,runMorse
       ,morseLetter
       ,morseBlobDict
       ,morseWord
       ,morseDictWord
       ) where

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

type Morse a = ReadP a
runMorse :: Morse a -> ReadS a
runMorse = readP_to_S

morseParsers :: [ReadP Char]
morseParsers = map build morseTable where
  build (x,enc) =
    do string enc
       return x
       
morseLetter :: Morse Char
morseLetter = choice morseParsers

-- Perform p until p fails
eager :: Morse a -> Morse [a]
eager p = do x <- many p
             fails p
             return x
             
fails :: Morse a -> Morse ()
fails p = do x <- (many1 p <++ return [])
             guard (null x)
             
mem :: String -> Dict -> Bool
mem x d =
  (x `elem` ["A","I","AM","ON","IN","OF","SO","IT"] || (length x >= 3))
  && S.member x d
             

morseBlobDict :: Dict -> Morse String
morseBlobDict d =
  do x <- many1 morseLetter
     guard $ mem x d
     return x

morseWord :: Morse String
morseWord = manyTill h eof where
  h = do x <- morseLetter
         eof +++ (char ' ' >> return ())
         skipSpaces
         return x
         
morseDictWord :: Dict -> Morse String
morseDictWord d =
  do x <- sepBy1 morseLetter (char ' ')
     guard (S.member x d)
     return x 

readInt :: String -> [(Int,String)]
readInt s = [(x,"")]
  where x = read s

wrd :: Morse String
wrd =  many1 $ satisfy isAlpha
