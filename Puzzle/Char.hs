module Puzzle.Char
       (ntol
       , lton
       , shift
       , vowel
       , vowel'
       , consonant
       , consonant'
       )
       where

import Data.Char

ntolBy :: Char -> Int -> Char
ntolBy r x = chr ( (ord r) + x - 1)

ltonBy :: Char -> Char -> Int
ltonBy r c = ord c - ord r + 1

ntol :: Int -- ^ value  1-26
        -> Char -- ^ letter 'A' - 'Z'
ntol = ntolBy 'A'

lton :: Char -- ^ letter 'A' - 'Z'
        -> Int -- ^ value 1-26
lton = ltonBy 'A'

-------------------- Caesar Shifting --------------------

-- | A class of (Caesar, say) shiftable objects. Should satisfy
-- | that shift is a group action of Int.
class Shift b where
  shift :: Int -> b -> b
  
shiftBy :: Char -> Int -> Char -> Char
shiftBy r n c = ntolBy r $ 1 + mod ((ltonBy r c) + n-1) 26
  
instance Shift Char where
  shift n c | isLower c = shiftBy 'a' n c
            | isUpper c = shiftBy 'A' n c
            | otherwise = c 

instance Shift a => Shift [a] where
  shift n = map (shift n)

vowelsL  = "AEIOU"
vowelsL' = "AEIOUY"

vowel = (`elem` vowelsL)
vowel' = (`elem` vowelsL')

consonant x = isAlpha x && isUpper x && not (vowel x)
consonant' x = isAlpha x && isUpper x && not (vowel' x)

