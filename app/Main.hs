module Main where
import QuadTree
import Physics
import Visualize
import Debug.Trace

doLoop :: QuadTree -> Double -> QuadTree
doLoop oldTree dt = newTree --traceShow newTree newTree
  where oldbodyList = toList oldTree
        updatedBodyList = map (approximateForce oldTree) oldbodyList
        movedBodyList = map (doTimeStep dt) updatedBodyList
        newTree = calcCOM $ fromList movedBodyList (getInfo oldTree)

au = (149.6 * (10^6) * 1000) :: Double

main :: IO ()
main = runSimulation smol doLoop

emptySmol = emptyQTree 0 200 0 200
b1' = Body 250 0 0 0 0
b2' = Body 10 500 0 0 125

sunMass :: Double
sunMass = 1.98892*(10^30)
sun = Body sunMass 0 0 0 0
mercury = Body (0.3301*(10^24) :: Double) (-0.387 * au) 0 0 (-47.36)
venus = Body (4.8685*(10^24) :: Double) (-0.723 * au) 0 0 (-35.02)
earth = Body (5.9742*(10^24) :: Double) (-1 * au) 0 0 (-29.783)
mars = Body (0.6417*(10^24) :: Double) (-1.524 * au) 0 0 (-24.07)

empty = emptyQTree (-5000) 5000 (-5000) 5000

smol = calcCOM $ insert b2' $ b1' `insert` empty

solarSystem = calcCOM $ insert mars $ insert earth $ insert venus $ insert mercury $ insert sun empty

bodyList = [(Body 5 (x * 15 + 100) (x * 15) 5 0) | x <- [0..100]]
bigTree = calcCOM $ insert b1' $ foldl (flip insert) empty bodyList

