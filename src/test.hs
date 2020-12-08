import QuadTree
import Physics
empty = emptyQTree 0 5000 0 5000
b1 = Body 5 7 7 100 100
b2 = Body 5 8 8 100 100 
b3 = Body 5 9 9 100 100 
filled = calcCOM $ insert b3 $ insert  b2 $ b1 `insert` empty

bodyList = [(Body 500 1 x 100 100) | x <- [0..100]]
bigTree = calcCOM $ foldl (flip insert) empty bodyList

emptySmol = emptyQTree 0 10 0 10
b1' = Body 5 2 2 100 100
b2' = Body 5 7 2 100 100
b3' = Body 5 2 7 100 100
b4' = Body 5 7 7 100 100

smol = calcCOM $ insert b4' $ insert b3' $ insert b2' $ b1' `insert` emptySmol
