module Main where
import QuadTree
import Physics
import Visualize
import GHC.Float

doLoop :: QuadTree -> Double -> QuadTree
doLoop oldTree dt = newTree
  where oldbodyList = toList oldTree
        updatedBodyList = map (approximateForce oldTree) oldbodyList
        movedBodyList = map (doTimeStep dt) updatedBodyList
        newTree = calcCOM $ fromList movedBodyList

type Model = (Float, Float)

main :: IO ()
main = simulate (InWindow "Nice Window" (1500, 1500) (10, 10)) 
       white 60 
       smol
       (\(qt) -> drawQuadTree  
       (\_ dt (qt) -> doLoop qt (float2Double dt))

emptySmol = emptyQTree 0 200 0 200
b1' = Body 250 0 0 0 0
b2' = Body 10 500 0 0 125

empty = emptyQTree 0 5000 0 5000

smol = calcCOM $ insert b2' $ b1' `insert` empty

bodyList = [(Body 5 (x * 15 + 100) (x * 15) 5 0) | x <- [0..100]]
bigTree = calcCOM $ insert b1' $ foldl (flip insert) empty bodyList

