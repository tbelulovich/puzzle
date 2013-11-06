{-# LANGUAGE NoMonomorphismRestriction #-}
module Puzzle.Diags where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.SVG
import Control.Applicative
import Control.Category ((>>>))



tile color = square 1 # fc color
                      # lw 0.05
                      # lc black


colorFromBit '0' = white
colorFromBit '1' = black

arrangeTiles = map (foldl1 (|||)) 
               >>> foldl1 (===)

tilesFromTable t = arrangeTiles $ map (map (tile .colorFromBit)) t

-- Output a diagram directly
testDiagram = tilesFromTable ["1111","1010","1100","1010","1001"]
test = renderSVG "/home/thobel/check.svg" (mkSizeSpec (Just 400) (Just 400)) testDiagram

-- If using as a standalone, do:
-- Usage -o <output> -w <width>

-- main =  defaultMain testDiagram
