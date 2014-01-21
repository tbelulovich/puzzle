{-# LANGUAGE NoMonomorphismRestriction #-}
module Puzzle.Diags where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.Colour.SRGB
import Control.Category ((>>>))

tile color = square 1 # fc color
                      # lw 0.05
                      # lc black

fromBit '0' = white
fromBit '1' = black

fromRGB (x,y,z) = RGB x y z

arrangeTiles = map (foldl1 (|||)) 
               >>> foldl1 (===)

tilesFromTable t = arrangeTiles $ map (map (tile . fromBit)) t

writeDiagram d (x,y) fn =
  renderSVG fn (mkSizeSpec (Just x) (Just y)) d
