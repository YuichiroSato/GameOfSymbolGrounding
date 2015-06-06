module Data.World(
  World,
  fromList,
  toList,
  toCellList,
  initialize,
  emptyWorld,
  writeCell,
  readCell) where

import Data.Cell
import Data.List (foldl')
import qualified Data.Vector as Vector
import Main.Setting
import System.Random

data Row = Row (Vector.Vector Cell) deriving (Eq, Show)
data World = World (Vector.Vector Row) deriving (Eq, Show)

fromList :: [((Int, Int), [Cell])] -> World
fromList xs = foldl' hoge emptyWorld xs
  where
    hoge w ((x, y), cs) = if null cs
      then w
      else writeCell w x y (head cs)

toList :: World -> [((Int, Int), Cell)]
toList w = foldl' hoge [] xys
  where
    hoge res (x, y) = ((x, y), (readCell w x y)) : res

toCellList :: World -> [Cell]
toCellList w = foldl' hoge [] xys
  where
    hoge res (x, y) = (readCell w x y) : res

initialize :: (RandomGen g) => g -> World
initialize gen = foldl override emptyWorld cells
  where
    override w ((x, y), c) = writeCell w x y c
    cells = recMakeCell xys gen []

emptyWorld :: World
emptyWorld = World $ Vector.fromList $ [(emptyColom y) | y <- [0..(ySize - 1)]]

emptyColom :: Int -> Row
emptyColom y = Row $ Vector.fromList $ [(emptyCell x y) | x <- [0..(xSize - 1)]]

recMakeCell :: (RandomGen g) => [(Int, Int)] -> g -> [((Int, Int), Cell)] -> [((Int, Int), Cell)]
recMakeCell [] _ res = res
recMakeCell (x:xs) gen res = recMakeCell xs gen2 ((x,c) : res)
  where
    (c, gen2) = randomCell (fst x) (snd x) gen

xys :: [(Int, Int)]
xys = [(i, j) | i <- [0..(xSize - 1)], j <- [0..(ySize - 1)]]

writeCell :: World -> Int -> Int -> Cell -> World
writeCell (World w) x y c = World $ w Vector.// [(y, writeRow (w Vector.! y) x c)]

writeRow :: Row -> Int -> Cell -> Row
writeRow (Row r) x c = Row $ r Vector.// [(x, c)]

readCell :: World -> Int -> Int -> Cell
readCell (World w) x y = readRow (w Vector.! y) x

readRow :: Row -> Int -> Cell
readRow (Row r) x = r Vector.! x
