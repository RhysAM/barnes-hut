module Main where
import QuadTree
import Physics
import Graphics.Gloss

doLoop :: QuadTree -> Int -> QuadTree
doLoop oldTree 0 = oldTree
doLoop oldTree n = doLoop newTree (n - 1)
  where oldbodyList = toList oldTree
        updatedBodyList = map (approximateForce oldTree) oldbodyList
        movedBodyList = map doTimeStep updatedBodyList
        newTree = calcCOM $ fromList movedBodyList

main :: IO ()
main = display window background drawing
 where
      window = InWindow "Tendies" (200, 200) (0, 0) 
      background = white 
      drawing = Circle 80