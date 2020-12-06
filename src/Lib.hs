module Lib where

thetaThreshold = 0
g = 1
timeStep = 0.5

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Body = Body {  mass :: Double
                 ,  xCord :: Double
                 ,  yCord :: Double
                 ,  xVel :: Double
                 ,  yVel :: Double
                 }

instance Eq Body where
  b1 == b2 = (xCord b1 == xCord b2) && (yCord b1 == yCord b2)

data CenterMass = CenterMass {  cMass :: Double
                             ,  cx :: Double
                             ,  cy :: Double
                             }

instance Show CenterMass where
    show (CenterMass ma xx yy) = "COM " ++ (show ma) ++ " @ " ++ "(" ++ (show xx) ++ ", " ++ (show yy) ++ ")"

data QuadInfo = QuadInfo {  xl :: Double
                         ,  xr :: Double
                         ,  yb :: Double
                         ,  yt :: Double
                         ,  com :: CenterMass
                         }

instance Show QuadInfo where
    show (QuadInfo xxl xxr yyb yyt com') = "QI[ X:" ++ (show xxl) ++ "-" ++ (show xxr) ++ ", Y:" ++ (show yyb) ++ "-" ++ (show yyt) ++ ", "++ (show com') ++ "]"

instance Show Body where
    show (Body m x y xVel yVel) = "body @ (" ++ (show x) ++ ", " ++ (show y) ++ ") -> mass: " ++ show m ++ ", vel: (" ++ (show xVel) ++ ", " ++ (show yVel) ++ ")"

data QuadTree = QuadTree QuadTree QuadTree QuadTree QuadTree QuadInfo
              | QuadNode (Maybe Body) QuadInfo

toList :: QuadTree -> [Body]
toList (QuadNode Nothing qi) = []
toList (QuadNode (Just b) qi) = [b]
toList (QuadTree nw ne sw se qi) = toList nw ++ toList ne ++ toList sw ++ toList se

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
traversePrint (QuadTree nw ne sw se qi) lvl = prInfo ++ travNW ++ travNE ++ travSW ++ travSE 
            where travNW = "\n" ++ replicate lvl '-' ++ traversePrint nw (lvl + 1)
                  travNE = "\n" ++ replicate lvl '-' ++ traversePrint ne (lvl + 1)
                  travSW = "\n" ++ replicate lvl '-' ++ traversePrint sw (lvl + 1)
                  travSE = "\n" ++ replicate lvl '-' ++ traversePrint se (lvl + 1)
                  prInfo = (if lvl /= 0 then "\\_ " else "") ++ show qi


instance Show QuadTree where
     show (QuadNode b qi) = show b ++ " " ++ show qi
     show qt = traversePrint qt 0

approximateForce :: QuadTree -> Body -> Body -- Run Barnes Hut
approximateForce (QuadNode Nothing qi) b = b -- nothing to compute
approximateForce (QuadNode (Just b1) qi ) b
  | b == b1 = b 
  | otherwise = updateVelocity b b1
approximateForce (QuadTree nw ne sw se qi) b
  | theta < thetaThreshold = updateVelocity b referenceMass-- Treat this quadrant as a single mass
  | otherwise = approximateForce nw $ approximateForce ne $ approximateForce sw $ approximateForce se b
  where xDiff = xCord b - (cx . com) qi
        yDiff = yCord b - (cy . com) qi
        distance = xDiff * xDiff + yDiff * yDiff
        s = xr qi - xl qi
        theta = s / (sqrt distance)
        referenceMass = Body ((cMass . com) qi) ((cx . com) qi) ((cy . com) qi) 0 0 -- Consider the COM a body for calculation

doTimeStep :: Body -> Body
doTimeStep b = b {xCord = xCord b + xVel b * timeStep, yCord = yCord b + yVel b * timeStep}

updateVelocity :: Body -> Body -> Body
updateVelocity bodyToUpdate otherBody 
  | bodyToUpdate == otherBody = bodyToUpdate
  | otherwise = bodyToUpdate {xVel = xVel bodyToUpdate +  xVelChange, yVel = yVel bodyToUpdate + yVelChange}
  where xDiff = xCord bodyToUpdate - xCord otherBody
        yDiff = yCord bodyToUpdate - yCord otherBody
        distance = xDiff * xDiff + yDiff * yDiff
        angleToBody = atan2 yDiff xDiff
        xVelChange = g * cos(angleToBody) * (mass bodyToUpdate * mass otherBody / distance)
        yVelChange = g * sin(angleToBody) * (mass bodyToUpdate * mass otherBody / distance)

fromList :: [Body] -> QuadInfo -> QuadTree
fromList bs qi = foldl (flip insert) empty bs where empty = emptyQTree (xl qi) (xr qi) (yb qi) (yt qi) 

doLoop :: QuadTree -> Int -> QuadTree
doLoop oldTree 0 = oldTree
doLoop oldTree n = doLoop newTree (n - 1)
  where oldbodyList = toList oldTree
        updatedBodyList = map (approximateForce oldTree) oldbodyList
        movedBodyList = map doTimeStep updatedBodyList
        newTree = calculateCOM $ fromList movedBodyList (getInfo oldTree)
