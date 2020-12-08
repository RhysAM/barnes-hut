module Visualize where

import Graphics.Gloss
import QuadTree

getCircle :: Float -> Picture
getCircle size = circle size

drawBody :: Body -> Picture
drawBody b = Translate x y (getCircle mass')
  where x = realToFrac $ xCord b
        y = realToFrac $ yCord b
        mass' = realToFrac $ mass b

drawQuadTree :: QuadTree -> Picture
drawQuadTree = pictures . map drawBody . toList