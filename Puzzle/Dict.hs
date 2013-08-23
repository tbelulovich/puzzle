module Puzzle.Dict where

import Control.Arrow
import qualified Data.Set as S
import Data.Char
import Control.Applicative
import Control.Monad

type Dict = S.Set String
        
buildDict :: [String] -> Dict
buildDict =
  filter (all isAlpha) >>>
  map (map toUpper) >>>
  S.fromList 

dictFromFile :: FilePath -> IO Dict
dictFromFile fp = 
  do x <- lines <$> readFile fp
     return $ buildDict x 

defaultDict :: IO Dict
defaultDict = 
  dictFromFile "/usr/share/dict/words"