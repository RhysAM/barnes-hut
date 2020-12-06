import Lib
empty = emptyQTree 0 100 0 100
b1 = Body 5 7 7 100 100
b2 = Body 5 8 8 100 100 
b3 = Body 5 9 9 100 100 
filled = insert b3 $ insert  b2 $ b1 `insert` empty

bodyList = [(Body 10 x x 100 100) | x <- [0..100]]
bigTree = calculateCOM $ foldl (flip insert) empty bodyList
