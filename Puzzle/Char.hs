module Puzzle.Char (ntol, lton, shift) where
import Data.Char

ntolBy r x = chr ( (ord r) + x - 1) 
ltonBy r c = ord c - ord r + 1

ntol = ntolBy 'A'
lton = ltonBy 'A'

-------------------- Caesar Shifting --------------------

--- A class of (Caesar, say) shiftable objects. Should satisfy
--- that shift is a group action of Int.
class Shift b where
  shift :: Int -> b -> b
  
shiftBy r n c = ntolBy r $ 1 + mod ((ltonBy r c) + n-1) 26
  
instance Shift Char where
  shift n c | isLower c = shiftBy 'a' n c
            | isUpper c = shiftBy 'A' n c
            | otherwise = c 

instance Shift a => Shift [a] where
  shift n = map (shift n)
