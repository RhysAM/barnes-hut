module Main where
import QuadTree
import Physics
import Visualize
import Debug.Trace
import Control.Parallel.Strategies(parMap, runEval, rdeepseq, parListChunk, using)

nChunks :: Int
nChunks = 16

doLoopParMap :: QuadTree -> Double -> QuadTree
doLoopParMap oldTree dt = newTree --traceShow newTree newTree
  where oldbodyList = toList oldTree
        updatedBodyList = parMap rdeepseq (\b -> approximateForce oldTree b dt) oldbodyList
        movedBodyList = parMap rdeepseq (doTimeStep dt) updatedBodyList
        newTree = calcCOM $ fromList movedBodyList (getInfo oldTree)

doLoopParListChunks :: QuadTree -> Double -> QuadTree
doLoopParListChunks oldTree dt = newTree --traceShow newTree newTree
  where oldbodyList = toList oldTree
        updatedBodyList = map (\b -> approximateForce oldTree b dt) oldbodyList `using` parListChunk nChunks rdeepseq
        movedBodyList = map (doTimeStep dt) updatedBodyList `using` parListChunk nChunks rdeepseq
        newTree = calcCOM $ fromList movedBodyList (getInfo oldTree)

doLoop :: QuadTree -> Double -> QuadTree
doLoop oldTree dt = newTree --traceShow newTree newTree
  where oldbodyList = toList oldTree
        updatedBodyList = map (\b -> approximateForce oldTree b dt) oldbodyList
        movedBodyList = map (doTimeStep dt) updatedBodyList
        newTree = calcCOM $ fromList movedBodyList (getInfo oldTree)

au = (149.6 * (10^6) * 1000) :: Double

runLoop :: Int -> (QuadTree -> Double -> QuadTree) -> QuadTree -> Double -> [QuadTree]
runLoop n loopFunc startTree dt = take n $ iterate (\qt -> loopFunc qt dt) startTree

main :: IO ()
main = do 
    let x = runLoop 5001 doLoopParMap smol (0.5)
    print (x !! (5001 - 1)) -- very ugly way to force evaluation
    return () -- runSimulation smol doLoopParMap --(\qt _ -> qt) --doLoop

emptySmol = emptyQTree 0 200 0 200
b1' = Body 5000000 0 0 0 0 1
b2' = Body 10 500 0 0 125 200

b1Orbiters = map (\x -> generateOrbiter b1' x 5) [500,1500..100000]

-- sunMass :: Double
-- sunMass = 1.98892*(10^30)
-- sun = Body sunMass 0 0 0 0
-- mercury = Body (0.3301*(10^24) :: Double) (-0.387 * au) 0 0 (-47.36)
-- venus = Body (4.8685*(10^24) :: Double) (-0.723 * au) 0 0 (-35.02)
-- earth = Body (5.9742*(10^24) :: Double) (-1 * au) 0 0 (-29.783)
-- mars = Body (0.6417*(10^24) :: Double) (-1.524 * au) 0 0 (-24.07)

empty = emptyQTree (-20000) 20000 (-20000) 20000

smol = calcCOM $ insert b1' $ fromList b1Orbiters (getInfo empty)

-- solarSystem = calcCOM $ insert mars $ insert earth $ insert venus $ insert mercury $ insert sun empty

bodyList = [(Body 5 (x * 15 + 100) (x * 15) 0 0 10) | x <- [0..5]]
bigTree = calcCOM $ insert b1' $ foldl (flip insert) empty bodyList

