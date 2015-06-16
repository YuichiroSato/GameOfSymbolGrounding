module Data.Cell(
  Cell(..),
  Type(..),
  External(..),
  Internal(..),
  isEmptyCell,
  isPlant,
  isHerbivore,
  isCarnivore,
  isDead,
  emptyCell,
  plant,
  herbivore,
  carnivore,
  randomCell,
  move,
  consume,
  birth,
  eat,
  resolve
  ) where

import Control.Monad.State (runState)
import Main.Setting (xSize, ySize)
import Data.RandomState (rand4)
import System.Random (RandomGen)

data Cell = Cell External Internal deriving (Eq, Show)
data Type = Empty | Plant | Herbivore | Carnivore deriving (Eq, Enum, Show)
data External = External {
  cellType :: Type,
  xy :: (Int, Int),
  cellColor :: Int
  } deriving (Eq, Show)
data Internal = Internal {
  vx :: Int,
  vy :: Int,
  life :: Int
  } deriving (Eq, Show)

x :: External -> Int
x ext = fst $ xy ext

y :: External -> Int
y ext = snd $ xy ext

isEmptyCell :: Cell -> Bool
isEmptyCell (Cell ext _) = cellType ext == Empty

isPlant :: Cell -> Bool
isPlant (Cell ext _) = cellType ext == Plant

isHerbivore :: Cell -> Bool
isHerbivore (Cell ext _) = cellType ext == Herbivore

isCarnivore :: Cell -> Bool
isCarnivore (Cell ext _) = cellType ext == Carnivore

isDead :: Cell -> Bool
isDead (Cell _ int) = life int < 0

plant :: Int -> Int -> Int -> Int -> Int -> Cell
plant x0 y0 c _vx _vy = Cell (External Plant (x0, y0) c) (Internal _vx _vy 100)

herbivore :: Int -> Int -> Int -> Int -> Int -> Cell
herbivore x0 y0 c _vx _vy = Cell (External Herbivore (x0, y0) c) (Internal _vx _vy 100)

carnivore :: Int -> Int -> Int -> Int -> Int -> Cell
carnivore x0 y0 c _vx _vy = Cell (External Carnivore (x0, y0) c) (Internal _vx _vy 100)

emptyCell :: Int -> Int -> Cell
emptyCell x0 y0 = Cell (External Empty (x0,y0) 1) (Internal 0 0 1)

randomCell :: (RandomGen g) => Int -> Int -> g -> (Cell, g)
randomCell x0 y0 gen = (Cell ext int, newGen)
  where
    int = randomInternal t r2 r3
    ext@(External t _ _) = randomExternal r1 (x0, y0) r4
    ((r1, r2, r3, r4), newGen) = runState rand4 gen

randomExternal :: Int -> (Int, Int) -> Int -> External
randomExternal i xy0 c = case i `mod` 7 of
  0 -> External Empty xy0 10
  1 -> External Plant xy0 10
  2 -> External Herbivore xy0 c
  3 -> External Carnivore xy0 c
  _ -> External Plant xy0 10

randomInternal :: Type -> Int -> Int -> Internal
randomInternal t rvx rvy = case t of
  Empty -> Internal 0 0 1
  Plant -> Internal ((rvx `mod` 5) - 2) ((rvy `mod` 5) - 2) 10
  Herbivore -> Internal ((rvx `mod` 5) - 2) ((rvy `mod` 5) - 2) 100
  Carnivore -> Internal ((rvx `mod` 7) - 3) ((rvy `mod` 7) - 3) 100

move :: Cell -> Cell
move c@(Cell ext int)
  | isMovable ext = setXY c newx newy
  | otherwise = c
    where
      newx = (x ext + vx int) `mod` xSize
      newy = (y ext + vy int) `mod` ySize

isMovable :: External -> Bool
isMovable ext = cellType ext == Herbivore || cellType ext == Carnivore

setXY :: Cell -> Int -> Int -> Cell
setXY (Cell (External t _ c) int) x0 y0 = Cell (External t (x0, y0) c) int

addLife :: Cell -> Int -> Cell
addLife (Cell ext int) l = Cell ext (Internal (vx int) (vy int) (l + (life int)))

consume :: Cell -> Cell
consume c@(Cell ext _) = case cellType ext of
  Empty -> c
  Plant -> addLife c 10
  Herbivore -> addLife c (-1)
  Carnivore -> addLife c (-2)

eat :: [Cell] -> [Cell]
eat cs
  | length carns == 1 && 0 < length herbs = [(addLife (head carns) (100 * (length herbs)))]
  | length herbs == 1 && 0 < length plants = [(addLife (head herbs) (100 * (length plants)))]
  | otherwise = cs
    where
      plants = pickType cs Plant
      herbs = pickType cs Herbivore
      carns = pickType cs Carnivore

pickType :: [Cell] -> Type -> [Cell]
pickType css t = filter (\(Cell ext _) -> cellType ext == t) css

birth :: Cell -> (Cell, Maybe Cell)
birth c = if bornCheck c
  then (payBirthCost c, Just $ bornCell c)
  else (c, Nothing)

bornCheck :: Cell -> Bool
bornCheck (Cell ext int)
  | cellType ext == Plant = 100 < life int
  | cellType ext == Herbivore = 100 < life int
  | cellType ext == Carnivore = 100 < life int
  | otherwise = False

payBirthCost :: Cell -> Cell
payBirthCost c = addLife c (-100)

bornCell :: Cell -> Cell
bornCell (Cell ext int)
  | cellType ext == Plant = plant newx newy newc newvx newvy
  | cellType ext == Herbivore = herbivore x0 y0 newc newvx newvy
  | cellType ext == Carnivore = carnivore x0 y0 newc newvx newvy
  | otherwise = emptyCell newx newy
    where
      x0 = x ext
      y0 = y ext
      newx = (x0 + vx int) `mod` xSize
      newy = (y0 + vy int) `mod` ySize
      newvx = (-1 * (vx int))
      newvy = (-1 * (vy int))
      newc = cellColor ext

resolve :: [Cell] -> [Cell]
resolve cs
  | length bios == 1 = bios
  | otherwise = []
    where
      bios = filter (\c -> not $ isEmptyCell c) cs
