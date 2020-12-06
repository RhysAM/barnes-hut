import Lib
empty = emptyQTree 0 5000 0 5000
b1 = Body 5 7 7 100 100
b2 = Body 5 8 8 100 100 
b3 = Body 5 9 9 100 100 
filled = calculateCOM $ insert b3 $ insert  b2 $ b1 `insert` empty

bodyList = [(Body 500 1 x 100 100) | x <- [0..100]]
bigTree = calculateCOM $ foldl (flip insert) empty bodyList
