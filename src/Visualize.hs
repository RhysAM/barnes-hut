module Visualize where

import Graphics.Gloss
import QuadTree
import GHC.Float

getCircle :: Float -> Picture
getCircle size = circleSolid size

drawBody :: Body -> Picture
drawBody b = Color white $ Translate x y (getCircle (realToFrac $ radius b))
    where x = realToFrac $ xCord b
          y = realToFrac $ yCord b
          mass' = realToFrac $ mass b

drawQuadTree :: QuadTree -> [Picture] -> [Picture]
drawQuadTree (QuadNode (Nothing) qi) pics = (drawBox qi) : pics
drawQuadTree (QuadNode (Just b) qi) pics = (drawBox qi) : (drawBody b) : pics
drawQuadTree qt@(QuadTree nw ne sw se qi) pics = (drawBox qi) : (foldQuads drawQuadTree pics) qt ++ pics

drawBox :: QuadInfo -> Picture
drawBox qi = Color green $ Translate x y (rectangleWire (realToFrac $ (xr qi - xl qi)) (realToFrac $ (yt qi - yb qi)))
  where x = realToFrac $ (xr qi + xl qi) / 2
        y = realToFrac $ (yt qi + yb qi) / 2 


runSimulation :: QuadTree -> (QuadTree -> Double -> QuadTree) -> IO ()
runSimulation qt updateFunc = simulate (InWindow "Barnes-Hut Simulation" (1500, 1500) (10, 10)) 
                              black 60
                              qt
                              (\(qt) -> pictures $ drawQuadTree qt [])
                              (\_ dt (qt) -> updateFunc qt (float2Double dt))
