module Data.Cell where

import Graphics.Colors
import Main.Setting
import System.Random

data Cell = Cell External Internal deriving (Eq)
instance Show Cell where
  show (Cell ext int) = (show ext) ++ " " ++ (show int)

data Type = Empty | Plant | Herbivore | Carnivore deriving (Eq, Enum, Show)
data External = External {
  cellType :: Type,
  xy :: (Int, Int),
  cellColor :: IO ()
  }
instance Eq External where
  (==) a b = (cellType a) == (cellType b) && (xy a) == (xy b)
instance Show External where
  show e = "cellType " ++ (show $ cellType e) ++ ", xy " ++ (show $ xy e)

data Internal = Internal {
  vx :: Int,
  vy :: Int,
  life :: Double
  } deriving (Eq, Show)

plantCell :: Int -> Int -> Cell
plantCell x y = Cell (External Plant (x, y) green) (Internal 0 0 100)

herbivoreCell :: Int -> Int -> Int -> Int -> Cell
herbivoreCell x y _vx _vy = Cell (External Herbivore (x, y) blue) (Internal _vx _vy 100)

carnivoreCell :: Int -> Int -> Int -> Int -> Cell
carnivoreCell x y _vx _vy = Cell (External Carnivore (x, y) red) (Internal _vx _vy 100)

randomCell :: (RandomGen g) => Int -> Int -> g -> (Cell, g)
randomCell x y gen = (Cell ext int, nextGen)
  where
    ext = External t (x, y) c
    int = randomInternal t r2 r3
    (t, c) = randomType r1
    (r1, r2, r3, nextGen) = makeRandom
    makeRandom = (rr1, rr2, rr3, gen3)
      where
        (rr1, gen1) = next gen
        (rr2, gen2) = next gen1
        (rr3, gen3) = next gen2

randomType :: Int -> (Type, IO ())
randomType i = case i `mod` 4 of
  0 -> (Empty, black)
  1 -> (Plant, green)
  2 -> (Herbivore, blue)
  3 -> (Carnivore, red)

randomInternal :: Type -> Int -> Int -> Internal
randomInternal t rx ry = case t of
  Empty -> Internal 0 0 0
  Plant -> Internal 0 0 100
  Herbivore -> Internal (rx `mod` 2) (ry `mod` 2) 100.0
  Carnivore -> Internal (rx `mod` 3) (ry `mod` 3) 100.0

emptyCell :: Int -> Int -> Cell
emptyCell x y = Cell (External Empty (x,y) black) (Internal 0 0 0.0)

move :: Cell -> Cell
move (Cell ext int) = Cell (External (cellType ext) newxy (cellColor ext)) int
  where
    newxy = (newx, newy)
    newx = ((fst $ xy ext) + vx int) `mod` xSize
    newy = ((snd $ xy ext) + vy int) `mod` ySize

moveTo :: Cell -> Int -> Int -> Cell
moveTo (Cell ext int) x y = Cell (External (cellType ext) (x, y) (cellColor ext)) int