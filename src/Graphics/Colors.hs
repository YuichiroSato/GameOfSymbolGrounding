module Graphics.Colors(
  red,
  pink,
  green,
  blue,
  aqua,
  black,
  gray) where

import Graphics.UI.GLUT

color3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3f r g b = color $ Color3 r g b

red :: IO ()
red = color3f 1 0 0

pink :: Int -> IO ()
pink i = color3f 1 c c
  where
    c = (fromIntegral (i `mod` 11)) / 10

green :: IO ()
green = color3f 0 1 0

blue :: IO ()
blue = color3f 0 0 1

aqua :: Int -> IO ()
aqua i = color3f c c 1
  where
    c = (fromIntegral (i `mod` 11)) / 10

black :: IO ()
black = color3f 0 0 0

gray :: IO ()
gray = color3f 0.3 0.3 0.3
