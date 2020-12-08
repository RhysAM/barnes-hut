module Main where
import QuadTree
import Physics

doLoop :: QuadTree -> Int -> QuadTree
doLoop oldTree 0 = oldTree
doLoop oldTree n = doLoop newTree (n - 1)
  where oldbodyList = toList oldTree
        updatedBodyList = map (approximateForce oldTree) oldbodyList
        movedBodyList = map doTimeStep updatedBodyList
        newTree = calcCOM $ fromList movedBodyList

main :: IO ()
main = putStrLn "Gang gang"


