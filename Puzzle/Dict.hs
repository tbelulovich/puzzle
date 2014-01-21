module Puzzle.Dict 
       where
       
import Control.Arrow
import qualified Data.Set as S
import Data.Char
import Control.Applicative

type Dict = S.Set String
        
member :: String -> Dict -> Bool
member = S.member

toList :: Dict -> [String]
toList = S.toList

fromList :: [String] -> Dict
fromList =
  filter (all isAlpha) 
  >>> map (map toUpper) 
  >>> S.fromList 

fromFile :: FilePath -> IO Dict
fromFile fp = 
  do x <- lines <$> readFile fp
     return $ fromList x 

stdDict :: IO Dict
stdDict = 
  fromFile "/usr/share/dict/words"
