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
barnesHutParMap oldTree dt = newTree
  where oldbodyList = toList oldTree
        updatedBodyList = parMap rdeepseq (\b -> approximateForce oldTree b dt) oldbodyList
        movedBodyList = map (doTimeStep dt) updatedBodyList
        newTree = calcCOM $ fromList movedBodyList (getInfo oldTree)

barnesHutParBufChunks :: Int -> QuadTree -> Double -> QuadTree
barnesHutParBufChunks cz oldTree dt = newTree
  where oldbodyList = toList oldTree
        updatedBodyList = concat (map (map (\b -> doTimeStep dt $ approximateForce oldTree b dt)) (chunksOf cz oldbodyList) `using` parBuffer 100 rdeepseq)
        newTree = calcCOM $ fromList updatedBodyList (getInfo oldTree)

barnesHutParListChunks :: Int -> QuadTree -> Double -> QuadTree
barnesHutParListChunks cz oldTree dt = newTree
  where oldbodyList = toList oldTree 
        newTree = fromList (map (\b -> doTimeStep dt $ approximateForce oldTree b dt) oldbodyList `using` parListChunk cz rdeepseq) (getInfo oldTree)

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
        newTree = calcCOM $ fromList movedBodyList (getInfo oldTree)

makeBHSystem :: Int -> Int -> QuadTree
makeBHSystem n spacing = calcCOM $ insert blackhole $ fromList orbiters (getInfo empty)
    where blackhole = Body 5000000 0 0 0 0 1
          orbiters = [(\x -> generateOrbiter blackhole (fromIntegral x) 10) (spacing + spacing * i) | i <- [0..(n-2)]]

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
                 " [-i <iterations> -n <numBodies> [pm|plc <chunk-size>|pb|pbc <chunk-size>]]"

main :: IO ()
main = do 
    args <- getArgs
    case args of
      [] -> runSimulation (makeBHSystem 150 1000) barnesHut -- Graphic Demo
      ["-i", its, "-n", nb] -> print $ simpleLoop (read its) barnesHut (bhs (read nb)) 0.5
      ["-i", its, "-n", nb, "pm"] -> print $ simpleLoop (read its) barnesHutParMap (bhs (read nb)) 0.5
      ["-i", its, "-n", nb, "plc", cz] -> print $ calcCOM $ simpleLoop (read its) (barnesHutParListChunks $ read cz) (bhs (read nb)) 0.5
      ["-i", its, "-n", nb, "pbc", cz] -> print $ simpleLoop (read its) (barnesHutParBufChunks $ read cz) (bhs (read nb)) 0.5
      ["-i", its, "-n", nb, "pb"] -> print $ simpleLoop (read its) barnesHutParBuffer (bhs (read nb)) 0.5
      _ -> doUsage
    where bhs nb' = makeBHSystem nb' 1000
