module Main where
import QuadTree
import System.Random
import Physics
import Visualize
import Control.Parallel.Strategies(parMap, rdeepseq, parListChunk, using, parBuffer, Eval)
import System.Environment (getArgs, getProgName)
import System.Exit
import Data.List.Split(chunksOf)

empty :: QuadTree
empty = emptyQTree (-20000) 20000 (-20000) 20000


barnesHutParMap :: QuadTree -> Double -> QuadTree
barnesHutParMap oldTree dt = newTree
  where oldbodyList = toList oldTree
        updatedBodyList = parMap rdeepseq (\b -> approximateForce oldTree b dt) oldbodyList
        movedBodyList = map (doTimeStep dt) updatedBodyList
        newTree = calcCOM $ fromList movedBodyList (getInfo oldTree)

barnesHutParBufChunks :: Int -> QuadTree -> Double -> QuadTree
barnesHutParBufChunks cz oldTree dt = newTree
  where oldbodyList = toList oldTree
        updatedBodyList = concat (map (map (\b -> doTimeStep dt $ approximateForce oldTree b dt)) (chunksOf cz oldbodyList) `using` parBuffer 100 rdeepseq)
        newTree = calcCOM $ fromListPar updatedBodyList (getInfo oldTree)

barnesHutParListChunks :: Int -> QuadTree -> Double -> QuadTree
barnesHutParListChunks cz oldTree dt = newTree
  where oldbodyList = toList oldTree 
        newTree = fromListPar (map (\b -> doTimeStep dt $ approximateForce oldTree b dt) oldbodyList `using` parListChunk cz rdeepseq) (getInfo oldTree)

barnesHutParBuffer :: QuadTree -> Double -> QuadTree
barnesHutParBuffer oldTree dt = newTree
  where oldbodyList = toList oldTree
        updatedBodyList = map (\b -> approximateForce oldTree b dt) oldbodyList `using` parBuffer 100 rdeepseq
        movedBodyList = map (doTimeStep dt) updatedBodyList
        newTree = calcCOM (fromList movedBodyList (getInfo oldTree))

barnesHut :: QuadTree -> Double -> QuadTree
barnesHut oldTree dt = newTree
  where oldbodyList = toList oldTree
        updatedBodyList = map (\b -> approximateForce oldTree b dt) oldbodyList
        movedBodyList = map (doTimeStep dt) updatedBodyList
        newTree = calcCOM $ fromListPar movedBodyList (getInfo oldTree)

makeBHSystem :: Int -> Int -> QuadTree
makeBHSystem n spacing = calcCOM $ insert blackhole $ fromList orbiters (getInfo empty)
    where blackhole = Body 5000000 0 0 0 0 1
          orbiters = [(\x -> generateOrbiter blackhole (fromIntegral x) 10) (spacing + spacing * i) | i <- [0..(n-2)]]

makeBHSystemRandom :: Int -> [Double] -> [Double] -> [Double] -> QuadTree
makeBHSystemRandom n radii angles masses = calcCOM $ insert blackhole $ fromList [generateOrbiterAngle blackhole radius' mass' angle' | (radius', mass', angle') <- combinedList] (getInfo empty)
    where blackhole = Body 500000000 0 0 0 0 1
          combinedList = take n $ zip3 radii angles masses

simpleLoop :: Int -> (QuadTree -> Double -> QuadTree) -> QuadTree -> Double -> QuadTree
simpleLoop n f tree dt
  | n > 0 = simpleLoop (n - 1) f (f (calcCOM tree) dt) dt
  | otherwise = calcCOM tree

simpleLoop' :: Int -> QuadTree -> Double -> Eval QuadTree
simpleLoop' n tree dt
  | n <= 0 = return tree
  | otherwise = do let oldBodyList = toList tree
                   newBodyList <- parListChunk 24 rdeepseq (map (\b -> doTimeStep dt $ approximateForce tree b dt) oldBodyList)
                   newBodyList' <- rdeepseq newBodyList
                   simpleLoop' (n - 1) (calcCOM $ fromList newBodyList' (getInfo tree)) dt

doUsage :: IO ()
doUsage = do progName <- getProgName
             die $ "usage: " ++ progName ++
                 "[-i <iterations> -n <numBodies> [pm|plc <chunk-size>|pb|pbc <chunk-size>]]"

randomlist :: Random a => a -> a -> IO [a]
randomlist a b = fmap (randomRs (a,b)) newStdGen

main :: IO ()
main = do 
    args <- getArgs
    case args of
      [] -> runSimulation (makeBHSystem 150 1000) barnesHut -- Graphic Demo
      ["-r", a, b, "-n", numBodies, "-m", maxMass] -> do radii <- randomlist (read a) (read b :: Double)
                                                         angles <- randomlist 0 (2 * pi :: Double)
                                                         masses <- randomlist 0 (read maxMass :: Double)
                                                         runSimulation (makeBHSystemRandom (read numBodies) radii angles masses) (barnesHutParListChunks 50)--(\qt _ -> qt)
      ["-i", its, "-n", nb] -> do radii <- randomlist (1000) (50000 :: Double)
                                  angles <- randomlist 0 (2 * pi :: Double)
                                  masses <- randomlist 0 (1000 :: Double)
                                  print $ simpleLoop (read its) barnesHut (makeBHSystemRandom (read nb) radii angles masses) 0.5
      ["-i", its, "-n", nb, "pm"] -> print $ simpleLoop (read its) barnesHutParMap (bhs (read nb)) 0.5
      ["-i", its, "-n", nb, "plc", cz] -> do radii <- randomlist (1000) (50000 :: Double)
                                             angles <- randomlist 0 (2 * pi :: Double)
                                             masses <- randomlist 0 (1000 :: Double)
                                             print $ simpleLoop (read its) (barnesHutParListChunks $ read cz) (makeBHSystemRandom (read nb) radii angles masses) 0.5
      ["-i", its, "-n", nb, "pbc", cz] -> do radii <- randomlist (1000) (50000 :: Double)
                                             angles <- randomlist 0 (2 * pi :: Double)
                                             masses <- randomlist 0 (1000 :: Double)
                                             print $ simpleLoop (read its) (barnesHutParBufChunks $ read cz) (makeBHSystemRandom (read nb) radii angles masses) 0.5
      ["-i", its, "-n", nb, "pb"] -> print $ simpleLoop (read its) barnesHutParBuffer (bhs (read nb)) 0.5
      _ -> doUsage
    where bhs nb' = makeBHSystem nb' 1000
