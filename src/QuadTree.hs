module QuadTree where

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
    show (CenterMass ma xx yy) = "COM " ++ show ma ++ " @ " ++ "(" ++ show xx ++ ", " ++ show yy ++ ")"

data QuadInfo = QuadInfo {  xl :: Double
                         ,  xr :: Double
                         ,  yb :: Double
                         ,  yt :: Double
                         ,  com :: CenterMass
                         }

instance Show QuadInfo where
    show (QuadInfo xxl xxr yyb yyt com') = "QI[ X:" ++ show xxl ++ "-" ++ show xxr ++ ", Y:" ++ show yyb ++ "-" ++ show yyt ++ ", "++ show com' ++ "]"

instance Show Body where
    show (Body m x y xVel yVel) = "body @ (" ++ show x ++ ", " ++ show y ++ ") -> mass: " ++ show m ++ ", vel: (" ++ show xVel ++ ", " ++ show yVel ++ ")"

data QuadTree = QuadTree QuadTree QuadTree QuadTree QuadTree QuadInfo
              | QuadNode (Maybe Body) QuadInfo


getCOMX :: QuadTree -> Double
getCOMX (QuadTree _ _ _ _ qi) = cx . com $ qi
getCOMX (QuadNode _ qi) = cx . com $ qi

getCOMY :: QuadTree -> Double
getCOMY (QuadTree _ _ _ _ qi) = cy . com $ qi
getCOMY (QuadNode _ qi) = cy . com $ qi

getCOMM :: QuadTree -> Double
getCOMM (QuadTree _ _ _ _ qi) = cMass . com $ qi
getCOMM (QuadNode _ qi) = cMass . com $ qi

toList :: QuadTree -> [Body]
toList (QuadNode Nothing _) = []
toList (QuadNode (Just b) _) = [b]
toList (QuadTree nw ne sw se _) = toList nw ++ toList ne ++ toList sw ++ toList se

fromList :: [Body] -> QuadTree
fromList bs = foldl (flip insert) empty bs 
    where empty = emptyQTree xl' xr' yb' yt' -- Dynamically calculate bounds of new Quadtree
          xl' = minimum $ map xCord bs
          xr' = maximum $ map xCord bs
          yb' = minimum $ map yCord bs
          yt' = maximum $ map yCord bs

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

mapQuads :: (QuadTree -> a) -> QuadTree -> [a]
mapQuads f qn@(QuadNode b qi) = [f qn]
mapQuads f qt@(QuadTree nw ne sw se qi) = [f nw, f ne, f sw, f se]

foldQuads :: (QuadTree -> a -> a) -> a -> QuadTree -> a
foldQuads f z qn@(QuadNode _ _) = f qn z
foldQuads f z qt@(QuadTree nw ne sw se _) = foldQuads f (foldQuads f (foldQuads f (foldQuads f z se) sw) ne) nw

inQuad :: QuadTree -> Body -> Bool
inQuad qt b = xl qi <= x && xr qi >= x && yt qi >= y && yb qi <= y
            where x = xCord b
                  y = yCord b
                  qi = getInfo qt

combineBodies :: Body -> Body -> Body
combineBodies b1 b2 = b1 {mass = mass b1 + mass b2, xVel = xVel b1 + xVel b2, yVel = yVel b1 + yVel b2}

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

traversePrint :: QuadTree -> Int -> String
traversePrint n@(QuadNode b qi) lvl = "\\_ " ++ show n
traversePrint qt@(QuadTree nw ne sw se qi) lvl = concat $ prInfo : branches 
    where branches = mapQuads (\q -> "\n" ++ replicate lvl '-' ++ traversePrint q (lvl + 1)) qt
          prInfo = (if lvl /= 0 then "\\_ " else "") ++ show qi

instance Show QuadTree where
     show (QuadNode b qi) = show b ++ " " ++ show qi
     show qt = traversePrint qt 0