module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Body = Body {  mass :: Double
                 ,  xCord :: Double
                 ,  yCord :: Double
                 ,  xVel :: Double
                 ,  yVel :: Double
                 }

data CenterMass = CenterMass {  cMass :: Double
                             ,  cx :: Double
                             ,  cy :: Double
                             } deriving Show

data QuadInfo = QuadInfo {  xl :: Double
                         ,  xr :: Double
                         ,  yb :: Double
                         ,  yt :: Double
                         ,  com :: CenterMass
                         } deriving Show

instance Show Body where
    show (Body m x y xVel yVel) = "body @ (" ++ (show x) ++ ", " ++ (show y) ++ ") -> mass: " ++ show m ++ ", xVel: " ++ (show xVel) ++ ", yVel: " ++ (show yVel)

data QuadTree = QuadTree QuadTree QuadTree QuadTree QuadTree QuadInfo
              | QuadNode (Maybe Body) QuadInfo

getInfo :: QuadTree -> QuadInfo
getInfo (QuadTree _ _ _ _ qi) = qi
getInfo (QuadNode _ qi) = qi

emptyQNode :: Double -> Double -> Double -> Double -> QuadTree
emptyQNode xl xr yb yt = QuadNode Nothing (QuadInfo xl xr yb yt (CenterMass 0 0 0))

emptyQTree :: Double -> Double -> Double -> Double -> QuadTree
emptyQTree xl xr yb yt = QuadTree nw ne sw se (QuadInfo xl xr yb yt (CenterMass 0 0 0))
                        where xm = (xr + xl) / 2
                              ym = (yt + yb) / 2
                              nw = emptyQNode xl xm ym yt
                              ne = emptyQNode xm xr ym yt
                              sw = emptyQNode xl xm yb ym
                              se = emptyQNode xm xr yb ym

combineBodies :: Body -> Body -> Body
combineBodies b1 b2 = b1 {mass = (mass b1 + mass b2), xVel = (xVel b1 + xVel b2), yVel = (yVel b1 + yVel b2)}

insert :: Body -> QuadTree -> QuadTree
insert b (QuadNode Nothing qi) = QuadNode (Just b) qi
insert b2 (QuadNode (Just b1) qi)
  | (xCord b1 == xCord b2) && (yCord b1 == yCord b2) = QuadNode (Just $ combineBodies b1 b2) qi 
  | otherwise = insert b2 $ insert b1 $ emptyQTree (xl qi) (xr qi) (yb qi) (yt qi)
insert b (QuadTree nw ne sw se qi)
  | inQuad nw b = QuadTree (insert b nw) ne sw se qi
  | inQuad ne b = QuadTree nw (insert b ne) sw se qi
  | inQuad sw b = QuadTree nw ne (insert b sw) se qi
  | inQuad se b = QuadTree nw ne sw (insert b se) qi
  | otherwise = error "Couldn't find QuadTree to insert body"

calculateCOM :: QuadTree -> QuadTree
calculateCOM (QuadNode Nothing qi) = QuadNode Nothing qi
calculateCOM (QuadNode (Just b) qi) = QuadNode (Just b) (qi {com = CenterMass (mass b) (xCord b) (yCord b)})
calculateCOM (QuadTree nw ne sw se qi) = QuadTree newNW newNE newSW newSE (qi {com = CenterMass newMass newX newY})
                where nwCOM = com $ getInfo newNW
                      nwMass = cMass nwCOM
                      nwX = cx nwCOM
                      nwY = cy nwCOM
                      neCOM = com $ getInfo newNE
                      neMass = cMass neCOM
                      neX = cx neCOM
                      neY = cy neCOM
                      swCOM = com $ getInfo newSW
                      swMass = cMass swCOM
                      swX = cx swCOM
                      swY = cy swCOM
                      seCOM = com $ getInfo newSE 
                      seMass = cMass seCOM
                      seX = cx seCOM
                      seY = cy seCOM
                      newMass = nwMass + neMass + swMass + seMass
                      newX = ((nwMass * nwX) + (neMass * neX) + (swMass * swX) + (seMass * seX)) / newMass
                      newY = ((nwMass * nwY) + (neMass * neY) + (swMass * swY) + (seMass * seY)) / newMass
                      newNW = calculateCOM nw
                      newNE = calculateCOM ne
                      newSW = calculateCOM sw
                      newSE = calculateCOM se
                      
inQuad :: QuadTree -> Body -> Bool
inQuad qt b = xl qi <= x && xr qi >= x && yt qi >= y && yb qi <= y
            where x = xCord b
                  y = yCord b
                  qi = getInfo qt

traversePrint :: QuadTree -> Int -> String
traversePrint n@(QuadNode b qi) lvl = "\\_ " ++ show n
traversePrint (QuadTree nw ne sw se qi) lvl = replicate lvl '-' ++ show qi ++ "\n" ++ replicate lvl '-' ++ traversePrint nw (lvl + 1) ++ "\n" ++ replicate lvl '-' ++ traversePrint ne (lvl + 1) ++ "\n" ++ replicate lvl '-' ++ traversePrint sw (lvl + 1) ++ "\n" ++ replicate lvl '-' ++ traversePrint se (lvl + 1)


instance Show QuadTree where
     show (QuadNode b qi) = show b ++ " " ++ show qi
     show qt = traversePrint qt 0



