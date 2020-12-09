module Visualize where

import Graphics.Gloss
import QuadTree
import GHC.Float

getCircle :: Float -> Picture
getCircle size = circle size

drawBody :: Body -> Picture
drawBody b = Translate x y (getCircle mass')
    where x = realToFrac $ xCord b
          y = realToFrac $ yCord b
          mass' = realToFrac $ mass b

drawQuadTree :: QuadTree -> Picture
drawQuadTree = pictures . map drawBody . toList

runSimulation :: QuadTree -> (QuadTree -> Double -> QuadTree) -> IO ()
runSimulation qt updateFunc = simulate (InWindow "Barnes-Hut Simulation" (1500, 1500) (10, 10)) 
                              white 60
                              qt
                              (\(qt) -> drawQuadTree qt)
                              (\_ dt (qt) -> updateFunc qt (float2Double dt))
