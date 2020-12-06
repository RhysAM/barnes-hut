module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Body = Body {  mass :: Integer
                 ,  xCord :: Integer
                 ,  yCord :: Integer
                 ,  velocity :: Integer
                 }

data CenterMass = CenterMass {  cMass :: Integer
                             ,  cx :: Integer
                             ,  cy :: Integer
                             } deriving Show

data QuadInfo = QuadInfo {  xl :: Integer
                         ,  xr :: Integer
                         ,  yb :: Integer
                         ,  yt :: Integer
                         ,  com :: CenterMass
                         } deriving Show

instance Show Body where
    show (Body m x y vel) = "body @ (" ++ (show x) ++ ", " ++ (show y) ++ ") -> mass: " ++ show m ++ ", vel: " ++ (show vel)

data QuadTree = QuadTree QuadTree QuadTree QuadTree QuadTree QuadInfo
              | QuadNode (Maybe Body) QuadInfo
              deriving Show

getInfo :: QuadTree -> QuadInfo
getInfo (QuadTree _ _ _ _ qi) = qi
getInfo (QuadNode _ qi) = qi

emptyQNode :: Integer -> Integer -> Integer -> Integer -> QuadTree
emptyQNode xl xr yb yt = QuadNode Nothing (QuadInfo xl xr yb yt (CenterMass 0 0 0))

emptyQTree :: Integer -> Integer -> Integer -> Integer -> QuadTree
emptyQTree xl xr yb yt = QuadTree nw ne sw se (QuadInfo xl xr yb yt (CenterMass 0 0 0))
                        where xm = (xr + xl) `div` 2
                              ym = (yt + yb) `div` 2
                              nw = emptyQNode xl xm ym yt
                              ne = emptyQNode xm xr ym yt
                              sw = emptyQNode xl xm yb ym
                              se = emptyQNode xm xr yb ym

insert :: Body -> QuadTree -> QuadTree
insert b (QuadNode Nothing qi) = QuadNode (Just b) qi
insert b2 (QuadNode (Just b1) qi) = insert b2 $ insert b1 $ emptyQTree (xl qi) (xr qi) (yb qi) (yt qi)
insert b (QuadTree nw ne sw se qi)
  | inQuad nw b = QuadTree (insert b nw) ne sw se qi
  | inQuad ne b = QuadTree nw (insert b ne) sw se qi
  | inQuad sw b = QuadTree nw ne (insert b se) sw qi
  | inQuad se b = QuadTree nw ne sw (insert b se) qi
  | otherwise = error "Couldn't find QuadTree to insert body"

calculateCOM :: QuadTree -> QuadTree
calculateCOM (QuadNode Nothing qi) = QuadNode Nothing qi
calculateCOM (QuadNode (Just b) qi) = QuadNode (Just b) (qi {com = CenterMass (mass b) (xCord b) (yCord b)})
calculateCOM (QuadTree nw ne sw se qi) = QuadTree nw ne sw se (qi {com = CenterMass newMass newX newY})
                where nwCOM = com $ getInfo nw
                      nwMass = cMass nwCOM
                      nwX = cx nwCOM
                      nwY = cy nwCOM
                      neCOM = com $ getInfo ne
                      neMass = cMass neCOM
                      neX = cx neCOM
                      neY = cy neCOM
                      swCOM = com $ getInfo sw
                      swMass = cMass swCOM
                      swX = cx swCOM
                      swY = cy swCOM
                      seCOM = com $ getInfo se 
                      seMass = cMass seCOM
                      seX = cx seCOM
                      seY = cy seCOM
                      newMass = nwMass + neMass + swMass + seMass
                      newX = ((nwMass * nwX) + (neMass * neX) + (swMass * swX) + (seMass * seX)) `div` newMass
                      newY = ((nwMass * nwY) + (neMass * neY) + (swMass * swY) + (seMass * seY)) `div` newMass
                      
inQuad :: QuadTree -> Body -> Bool
inQuad qt b = xl qi <= x && xr qi >= x && yt qi >= y && yb qi <= y
            where x = xCord b
                  y = yCord b
                  qi = getInfo qt

-- traversePrint :: QuadTree -> Integer -> String
-- traversePrint (Node b) _ = show b
-- traversePrint (QuadTree nw ne sw we) lvl = (if lvl == 0 then "\n" else "")  ++


-- instance Show QuadTree where
--     show (Node b) = show b
--     show qt = traversePrint qt 0



