module Graphics.View where

import Main.Setting
import Graphics.UI.GLUT
import Graphics.Colors
import Graphics.Shapes
import Data.World
import Data.Cell

import qualified Data.Map as Map

{--

 -1.0  dx             1.0
    -0.9,0.9      0.9,0.9
      ---------------
   dy | | | | | | | 
      ------------
      | | | | | |
      ---- (0,0)
      | |
     -0.9,-0.9   0.9,-0.9
  -1.0               1.0
--}

xStart :: Double
xStart = -0.9

yStart :: Double
yStart = -0.9

xEnd :: Double
xEnd = 0.9

yEnd :: Double
yEnd = 0.9

dx :: Double
dx = 1.8 / (realToFrac xSize)

dy :: Double
dy = 1.8 / (realToFrac ySize)

render :: World -> DisplayCallback
render w = do
  clear [ColorBuffer]
  renderPrimitive Quads $ do
    drawCells (toList w)
  renderPrimitive Lines $ do
    gray
    drawLattice xSize ySize
  flush

drawLattice :: Int -> Int -> IO ()
drawLattice x y = do
  mapM_ (\(s, e) -> lineD s e) $ latticePoints x y

latticePoints :: Int -> Int -> [((Double, Double), (Double, Double))]
latticePoints x y = (horizontalPoints x) ++ (verticalPoints y)

horizontalPoints :: Int -> [((Double, Double), (Double, Double))]
horizontalPoints size = do
  i <- [0..size]
  let x = xStart + dx * (realToFrac i)
  let sp = (x, yStart)
  let ep = (x, yEnd)
  return (sp, ep)

verticalPoints :: Int -> [((Double, Double), (Double, Double))]
verticalPoints size = do
  i <- [0..size]
  let y = yStart + dy * (realToFrac i)
  let sp = (xStart, y)
  let ep = (xEnd, y)
  return (sp, ep)

drawCells :: [((Int,Int), Cell)] -> IO ()
drawCells cells = do
  mapM_ (\(_,c) -> cell2rectangle c) cells
  return ()

cell2rectangle :: Cell -> IO ()
cell2rectangle (Cell ext _) = rectangleD x y dx dy c
  where
    (x, y) = getXY cx cy
    (cx, cy) = xy ext
    c = cellColor ext

getXY :: Int -> Int -> (Double, Double)
getXY x y = (rx, ry)
  where
    rx = xStart + (realToFrac x) * dx
    ry = yStart + (realToFrac y) * dy
