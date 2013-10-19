module Puzzle.Dict 
       where
       
import Control.Arrow
import qualified Data.Set as S
import Data.Char
import Control.Applicative
import Control.Monad

type Dict = S.Set String
        
dMember :: String -> Dict -> Bool
dMember = S.member

dToList :: Dict -> [String]
dToList = S.toList

dFromList :: [String] -> Dict
dFromList =
  filter (all isAlpha) 
  >>> map (map toUpper) 
  >>> S.fromList 

dFromFile :: FilePath -> IO Dict
dFromFile fp = 
  do x <- lines <$> readFile fp
     return $ dFromList x 

stdDict :: IO Dict
stdDict = 
  dFromFile "/usr/share/dict/words"