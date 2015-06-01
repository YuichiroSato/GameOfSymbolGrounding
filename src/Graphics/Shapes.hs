module Graphics.Shapes where

import Graphics.UI.GLUT
import Graphics.Colors

d2gl :: Double -> GLfloat
d2gl d = realToFrac d

i2gl :: Int -> GLfloat
i2gl i = fromIntegral i

vertex3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertex3f x y z = vertex $ Vertex3 x y z

vertex3d :: Double -> Double -> Double -> IO ()
vertex3d x y z = vertex3f (d2gl x) (d2gl y) (d2gl z)

vertex3i :: Int -> Int -> Int -> IO ()
vertex3i x y z = vertex3f (i2gl x) (i2gl y) (i2gl z)

vertex2f :: GLfloat -> GLfloat -> IO ()
vertex2f x y = vertex3f x y 0

vertex2d :: Double -> Double -> IO ()
vertex2d x y = vertex2f (d2gl x) (d2gl y)

vertex2i :: Int -> Int -> IO ()
vertex2i x y = vertex2f (i2gl x) (i2gl y)

rectangle :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO () -> IO ()
rectangle x y w h rectangleColor = do
  rectangleColor
  vertex2f x y
  vertex2f x (y + w)
  vertex2f (x + h) (y + w)
  vertex2f (x + h) y

rectangleD :: Double -> Double -> Double -> Double -> IO () -> IO ()
rectangleD x y w h rectangleColor = rectangle (d2gl x) (d2gl y) (d2gl w) (d2gl h) rectangleColor

rectangleI :: Int -> Int -> Int -> Int -> IO () -> IO ()
rectangleI x y w h rectangleColor = rectangle (i2gl x) (i2gl y) (i2gl w) (i2gl h) rectangleColor

emptyRectangle :: Double -> Double -> Double -> Double -> IO ()
emptyRectangle x y w h = rectangleD x y w h black

line :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
line startx starty endx endy = do
  vertex2f startx starty
  vertex2f endx endy

lineD :: (Double, Double) -> (Double, Double) -> IO ()
lineD (startx, starty) (endx, endy) = line (d2gl startx) (d2gl starty) (d2gl endx) (d2gl endy)
