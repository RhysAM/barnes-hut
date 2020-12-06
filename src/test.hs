import Lib
empty = emptyQTree 0 10 0 10
b1 = Body 5 7 7 100 100
b2 = Body 5 8 8 100 100 
b3 = Body 5 9 9 100 100 
filled = insert b3 $ insert  b2 $ b1 `insert` empty

-- bodyList = [(Body 10 x x 100 100) | x <- [1..5]]
-- bigTree = foldl (flip insert) empty bodyList
