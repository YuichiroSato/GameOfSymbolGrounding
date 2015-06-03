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
  life :: Int
  } deriving (Eq, Show)

isEmptyCell :: Cell -> Bool
isEmptyCell (Cell ext _) = cellType ext == Empty

isPlant :: Cell -> Bool
isPlant (Cell ext _) = cellType ext == Plant

isHerbivore :: Cell -> Bool
isHerbivore (Cell ext _) = cellType ext == Herbivore

isCarnivore :: Cell -> Bool
isCarnivore (Cell ext _) = cellType ext == Carnivore

plantCell :: Int -> Int -> Int -> Int -> Cell
plantCell x y _vx _vy = Cell (External Plant (x, y) green) (Internal _vx _vy 100)

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
randomType i = case i `mod` 7 of
  0 -> (Empty, black)
  1 -> (Plant, green)
  2 -> (Herbivore, blue)
  3 -> (Carnivore, red)
  _ -> (Plant, green)

randomInternal :: Type -> Int -> Int -> Internal
randomInternal t rx ry = case t of
  Empty -> Internal 0 0 1
  Plant -> Internal ((rx `mod` 3) - 1) ((ry `mod` 3) - 1) 10
  Herbivore -> Internal ((rx `mod` 5) - 2) ((ry `mod` 5) - 2) 100
  Carnivore -> Internal ((rx `mod` 7) - 3) ((ry `mod` 7) - 3) 100

emptyCell :: Int -> Int -> Cell
emptyCell x y = Cell (External Empty (x,y) black) (Internal 0 0 1)

move :: Cell -> Cell
move c@(Cell ext int)
  | cellType ext == Herbivore || cellType ext == Carnivore = Cell (External (cellType ext) newxy (cellColor ext)) int
  | otherwise = c
    where
      newxy = (newx, newy)
      newx = ((fst $ xy ext) + vx int) `mod` xSize
      newy = ((snd $ xy ext) + vy int) `mod` ySize

moveTo :: Cell -> Int -> Int -> Cell
moveTo (Cell ext int) x y = Cell (External (cellType ext) (x, y) (cellColor ext)) int

addLife :: Cell -> Int -> Cell
addLife (Cell ext int) l = Cell ext (Internal (vx int) (vy int) (l + (life int)))

consume :: Cell -> Cell
consume c@(Cell ext _) = case cellType ext of
  Empty -> c
  Plant -> addLife c 10
  Herbivore -> addLife c (-1)
  Carnivore -> addLife c (-2)

eat :: (Cell, Cell) -> (Cell, Cell)
eat (c@(Cell (External Herbivore _ _) _), (Cell (External Plant xy2 _) _)) = (addLife c 100, emptyCell (fst xy2) (snd xy2))
eat (c@(Cell (External Carnivore _ _) _), (Cell (External Herbivore xy2 _) _)) = (addLife c 100, emptyCell (fst xy2) (snd xy2))
eat t = t

bornCheck :: Cell -> Bool
bornCheck (Cell ext int)
  | cellType ext == Plant && 100 < life int = True
  | cellType ext == Herbivore && 100 < life int = True
  | cellType ext == Carnivore && 100 < life int = True
  | otherwise = False

born :: Cell -> Cell
born (Cell ext int)
  | cellType ext == Plant = plantCell newx newy newvx newvy
  | cellType ext == Herbivore = herbivoreCell newx newy newvx newvy
  | cellType ext == Carnivore = carnivoreCell newx newy newvx newvy
  | otherwise = emptyCell newx newy
    where
      newx = ((fst $ xy ext) + (vx int)) `mod` xSize
      newy = ((snd $ xy ext) + (vy int)) `mod` ySize
      newvx = (-1 * (vx int))
      newvy = (-1 * (vy int))
