module Visualize where

import Graphics.Gloss
import QuadTree
import GHC.Float

drawBody :: Body -> Picture
drawBody b = Color white $ Translate x y (circleSolid (realToFrac $ radius b))
    where x = realToFrac $ xCord b
          y = realToFrac $ yCord b

drawQuadTree :: QuadTree -> [Picture] -> [Picture]
drawQuadTree (QuadNode Nothing qi) pics = drawBox qi : pics
drawQuadTree (QuadNode (Just b) qi) pics = drawBox qi : drawBody b : pics
drawQuadTree qt@(QuadTree _ _ _ _ qi) pics = drawBox qi : foldQuads drawQuadTree pics qt ++ pics

drawBox :: QuadInfo -> Picture
drawBox qi = Color (greyN 0.5) $ Translate x y (rectangleWire (realToFrac (xr qi - xl qi)) (realToFrac (yt qi - yb qi)))
  where x = realToFrac (xr qi + xl qi) / 2
        y = realToFrac (yt qi + yb qi) / 2 


runSimulation :: QuadTree -> (QuadTree -> Double -> QuadTree) -> IO ()
runSimulation qt updateFunc = simulate (InWindow "Barnes-Hut Simulation" (1500, 1500) (10, 10)) 
                              black 60
                              qt
                              (\ qt' -> pictures $ drawQuadTree qt' [])
                              (\_ dt qt' -> updateFunc qt' (float2Double dt))
