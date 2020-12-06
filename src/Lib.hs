module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Body = Body {  mass :: Double
                 ,  xCord :: Double
                 ,  yCord :: Double
                 ,  velocity :: Double
                 }

data CenterMass = CenterMass {  cMass :: Double
                             ,  cx :: Double
                             ,  cy :: Double
                             }

data QuadInfo = QuadInfo {  xl :: Double
                         ,  xr :: Double
                         ,  yt :: Double
                         ,  yb :: Double
                         ,  com :: Maybe CenterMass
                         }

instance Show Body where
    show (Body m x y vel) = "body @ (" ++ (show x) ++ ", " ++ (show y) ++ ") -> mass: " ++ show m ++ ", vel: " ++ (show vel)

data QuadTree = QuadTree QuadTree QuadTree QuadTree QuadTree QuadInfo
              | QuadNode (Maybe Body) QuadInfo

getInfo :: QuadTree -> QuadInfo
getInfo (QuadTree _ _ _ _ qi) = qi
getInfo (QuadNode _ qi) = qi

emptyQNode :: Double -> Double -> Double -> Double -> QuadTree
emptyQNode xl xr yt yb = QuadNode Nothing (QuadInfo xl xr yt yb Nothing)

emptyQTree :: Double -> Double -> Double -> Double -> QuadTree
emptyQTree xl xr yt yb = QuadTree nw ne sw se (QuadInfo xl xr yt yb Nothing)
                        where xm = (xr - xl) / 2
                              ym = (yt - yb) / 2
                              nw = emptyQNode xl xm yt ym
                              ne = emptyQNode xm xr yt ym
                              sw = emptyQNode xl xm ym yb
                              se = emptyQNode xm xr ym yb

insert :: Body -> QuadTree -> QuadTree
insert b (QuadNode Nothing qi) = QuadNode (Just b) qi
insert b2 (QuadNode (Just b1) qi) = insert b2 $ insert b1 $ emptyQTree (xl qi) (xr qi) (yt qi) (yb qi)
insert b (QuadTree nw ne sw se qi)
  | inQuad nw b = insert b nw
  | inQuad ne b = insert b ne
  | inQuad sw b = insert b se
  | inQuad se b = insert b sw
  | otherwise = error "Couldn't find QuadTree to insert body"

calculateMass :: QuadTree -> QuadTree
calculateMass (QuadNode Nothing qi) = QuadNode Nothing qi
calculateMass (QuadNode (Just b) qi) = QuadNode (Just b) (qi {com = Just $ CenterMass (mass b) (xCord b) (yCord b)})
calculateMass (QuadTree nw ne sw se qi) = QuadTree nw ne sw se (qi {com = })

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



