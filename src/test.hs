import Lib
empty = emptyQTree 0 10 0 10
b1 = Body 5 2 2 100 100
b2 = Body 5 3 3 100 100 
filled = b1 `insert` empty
filled2 = b2 `insert` filled

bodyList = [(Body 10 x x 100 100) | x <- [1..5]]
bigTree = foldl (flip insert) empty bodyList