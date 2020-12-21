module Main where
import QuadTree
import Physics
import Visualize
import Control.Parallel.Strategies(parMap, rdeepseq, parListChunk, using, parBuffer, Eval)
import System.Environment (getArgs, getProgName)
import System.Exit
import Data.List.Split(chunksOf)

empty :: QuadTree
empty = emptyQTree (-20000) 20000 (-20000) 20000

barnesHutParMap :: QuadTree -> Double -> QuadTree
barnesHutParMap oldTree dt = newTree --traceShow newTree newTree
  where oldbodyList = toList oldTree
        updatedBodyList = parMap rdeepseq (\b -> approximateForce oldTree b dt) oldbodyList
        movedBodyList = map (doTimeStep dt) updatedBodyList--parMap rdeepseq (doTimeStep dt) updatedBodyList
        newTree = calcCOM $ fromList movedBodyList (getInfo oldTree)

barnesHutParBufChunks :: Int -> QuadTree -> Double -> QuadTree
barnesHutParBufChunks cz oldTree dt = newTree --traceShow newTree newTree
  where oldbodyList = toList oldTree
        updatedBodyList = concat (map (map (\b -> doTimeStep dt $ approximateForce oldTree b dt)) (chunksOf cz oldbodyList) `using` parBuffer 100 rdeepseq)
        -- movedBodyList = map (doTimeStep dt) updatedBodyList --`using` parListChunk nChunks rdeepseq
        newTree = calcCOM $ fromList updatedBodyList (getInfo oldTree)

barnesHutParListChunks :: Int -> QuadTree -> Double -> QuadTree
barnesHutParListChunks cz oldTree dt = newTree
  where oldbodyList = toList oldTree 
        newTree = fromList (map (\b -> doTimeStep dt $ approximateForce oldTree b dt) oldbodyList `using` parListChunk cz rdeepseq) (getInfo oldTree)
        -- newTree = calcCOM $ fromList updatedBodyList (getInfo oldTree)

barnesHutParBuffer :: QuadTree -> Double -> QuadTree
barnesHutParBuffer oldTree dt = newTree --traceShow newTree newTree
  where oldbodyList = toList oldTree
        updatedBodyList = map (\b -> approximateForce oldTree b dt) oldbodyList `using` parBuffer 100 rdeepseq
        movedBodyList = map (doTimeStep dt) updatedBodyList --`using` parListChunk nChunks rdeepseq
        newTree = calcCOM (fromList movedBodyList (getInfo oldTree))

barnesHut :: QuadTree -> Double -> QuadTree
barnesHut oldTree dt = newTree --traceShow newTree newTree
  where oldbodyList = toList oldTree
        updatedBodyList = map (\b -> approximateForce oldTree b dt) oldbodyList
        movedBodyList = map (doTimeStep dt) updatedBodyList
        newTree = calcCOM $ fromList movedBodyList (getInfo oldTree)

makeBHSystem :: Int -> Int -> QuadTree
makeBHSystem n spacing = calcCOM $ insert blackhole $ fromList [(\x -> generateOrbiter blackhole (fromIntegral x) 10) (spacing + spacing * i) | i <- [0..(n-2)]] (getInfo empty)
    where blackhole = Body 500000000 0 0 0 0 1

simpleLoop :: Int -> (QuadTree -> Double -> QuadTree) -> QuadTree -> Double -> QuadTree
simpleLoop n f tree dt
  | n > 0 = simpleLoop (n - 1) f (f (calcCOM tree) dt) dt
  | otherwise = calcCOM tree

simpleLoop' :: Int -> QuadTree -> Double -> Eval QuadTree
simpleLoop' n tree dt
  | n <= 0 = return tree
  | otherwise = do let oldBodyList = toList tree
                             -- rdeepseq oldBodyList
                   newBodyList <- parListChunk 24 rdeepseq (map (\b -> doTimeStep dt $ approximateForce tree b dt) oldBodyList)
                   newBodyList' <- rdeepseq newBodyList
                   simpleLoop' (n - 1) (calcCOM $ fromList newBodyList' (getInfo tree)) dt

doUsage :: IO ()
doUsage = do progName <- getProgName
             die $ "usage: " ++ progName ++
                 " [-i <iterations> -n <numBodies> [pm|plc <chunk-size>|pb|pbc <chunk-size>]]"

main :: IO ()
main = do 
    args <- getArgs
    case args of
      [] -> runSimulation (makeBHSystem 150 1000)  barnesHut
      ["-i", iters, "-n", numBodies] -> print $ simpleLoop (read iters) barnesHut (makeBHSystem (read numBodies) 1000) 0.5
      ["-i", iters, "-n", numBodies, "pm"] -> print $ simpleLoop (read iters) barnesHutParMap (makeBHSystem (read numBodies) 1000) 0.5
      ["-i", iters, "-n", numBodies, "plc", cz] -> print $ calcCOM $ simpleLoop (read iters) (barnesHutParListChunks $ read cz) (makeBHSystem (read numBodies) 1000) 0.5
      ["-i", iters, "-n", numBodies, "pbc", cz] -> print $ simpleLoop (read iters) (barnesHutParBufChunks $ read cz) (makeBHSystem (read numBodies) 1000) 0.5
      ["-i", iters, "-n", numBodies, "pb"] -> print $ simpleLoop (read iters) barnesHutParBuffer (makeBHSystem (read numBodies) 1000) 0.5
      _ -> doUsage
