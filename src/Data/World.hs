module Data.World where

import Data.Cell
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Vector as Vector
import Main.Setting
import System.Random

data Colom = Colom (Vector.Vector [Cell]) deriving (Eq, Show) --row
data World = World (Vector.Vector Colom) deriving (Eq, Show)

margeWorld :: World -> World -> World
margeWorld w1 w2 = foldl iter w2 xys
  where
    iter w (x, y) = insertCells w x y $ getCell w1 x y

insert2Colom :: Colom -> Int -> Cell -> Colom
insert2Colom (Colom col) x c = Colom $ col Vector.// diff
  where
    diff = [(x, c : cs)]
    cs = col Vector.! x

insert2World :: World -> Int -> Int -> Cell -> World
insert2World (World cols) x y c = World $ cols Vector.// diff
  where
    diff = [(y, insert2Colom col x c)]
    col = cols Vector.! y

insertCells :: World -> Int -> Int -> [Cell] -> World
insertCells w x y cs = foldl (\ww c -> insert2World ww x y c) w cs

getCell :: World -> Int -> Int -> [Cell]
getCell (World cols) x y = col Vector.! x
  where
    (Colom col) = cols Vector.! y

removeCell :: Colom -> Int -> Cell -> Colom
removeCell (Colom col) x c = Colom $ col Vector.// diff
  where
    diff = [(x, newcs)]
    newcs = filter (\hoge -> hoge /= c) cs
    cs = col Vector.! x
    
deleteCell :: World -> Int -> Int -> Cell -> World
deleteCell (World cols) x y c = World $ cols Vector.// diff
  where
    diff = [(y, removeCell col x c)]
    col = cols Vector.! x

replaceCell :: World -> Int -> Int -> Cell -> Cell -> World
replaceCell w x y from to = insert2World (deleteCell w x y from) x y to

overrideColom :: Colom -> Int -> Cell -> Colom
overrideColom (Colom col) x c = Colom $ col Vector.// [(x, [c])]

overrideColoms :: Colom -> Int -> [Cell] -> Colom
overrideColoms (Colom col) x cs = Colom $ col Vector.// [(x, cs)]

overrideCell :: World -> Int -> Int -> Cell -> World
overrideCell (World cols) x y c = World $ cols Vector.// diff
  where
    diff = [(y, overrideColom (cols Vector.! y) x c)]

overrideCells :: World -> Int -> Int -> [Cell] -> World
overrideCells (World cols) x y cs = World $ cols Vector.// diff
  where
    diff = [(y, overrideColoms (cols Vector.! y) x cs)]

initialize :: (RandomGen g) => g -> World
initialize gen = foldl override emptyWorld cells
  where
    override w ((x, y), c) = overrideCell w x y c
    cells = recMakeCell xys gen []

emptyWorld :: World
emptyWorld = World $ Vector.fromList $ [(emptyColom y) | y <- [0..(ySize - 1)]]

emptyColom :: Int -> Colom
emptyColom y = Colom $ Vector.fromList $ [[(emptyCell x y)] | x <- [0..(xSize - 1)]]

recMakeCell :: (RandomGen g) => [(Int, Int)] -> g -> [((Int, Int), Cell)] -> [((Int, Int), Cell)]
recMakeCell [] _ res = res
recMakeCell (x:xs) gen res = recMakeCell xs gen2 ((x,c) : res)
  where
    (c, gen2) = randomCell (fst x) (snd x) gen

toMap :: World -> Map.Map (Int, Int) Cell
toMap w = Map.fromList ls
  where
    ls = iAdd [] xys
    makeMap (c@(Cell ext _):_) = ((xy ext), c)
    iAdd res [] = res
    iAdd res (_xy:_xys) = iAdd (m:res) _xys
      where
        m = makeMap $ getCell w x y
        x = fst _xy
        y = snd _xy

xys :: [(Int, Int)]
xys = [(i, j) | i <- [0..(xSize - 1)], j <- [0..(ySize - 1)]]

getCells :: World -> Map.Map (Int,Int) Cell
getCells w = toMap w

cellsSize :: World -> Int
cellsSize w = List.length $ Map.toList $ getCells w

update :: World -> World
update w = updateStrategy $ timeEvolution w

timeEvolution :: World -> World
timeEvolution w = removeDuplicateCells $ removeDeadCell $ makeBirth $ eatAnimal $ moveAnimal $ consumeCosts w

updateStrategy :: World -> World
updateStrategy w = w

consumeCosts :: World -> World
consumeCosts w = foldl iter w xys
  where
    iter res (x, y) = overrideCells res x y cells
      where
        cells = map consume (getCell w x y)

makeBirth :: World -> World
makeBirth w = margeWorld payBirthCost bornWorld
  where
    payBirthCost = foldl iter2 w xys
      where
        pay c = if bornCheck c
          then addLife c (-100)
          else c
        iter2 res (x, y) = overrideCells res x y $ map pay cells
          where
            cells = getCell w x y
    bornWorld = foldl iter emptyWorld xys
      where
        iter res (x, y) = foldl insert res borns
          where
            insert ww mayc = case mayc of
              Just c -> insert2World ww x y c
              Nothing -> ww
            borns = map makeBorn cells
            cells = getCell w x y

makeBorn :: Cell -> Maybe Cell
makeBorn c = if not $ bornCheck c
  then Nothing
  else Just $ born c

moveAnimal :: World -> World
moveAnimal w = makeNewCols xys emptyWorld
  where
    makeNewCols [] res = res
    makeNewCols (_xy:_xys) res = makeNewCols _xys updated
      where
        updated = moveCell w _xy res

moveCell :: World -> (Int, Int) -> World -> World
moveCell w (x, y) res = foldl oneStep res movedCells
  where
    movedCells = map move cells
    cells = getCell w x y

oneStep :: World -> Cell -> World
oneStep w c@(Cell ext _) = insert2World w modx mody c
  where
    modx = (fst (xy ext)) `mod` xSize
    mody = (snd (xy ext)) `mod` ySize

eatAnimal :: World -> World
eatAnimal w = foldl iEat w xys

iEat :: World -> (Int, Int) -> World
iEat w (x, y)
  | length carns == 1 && 0 < length herbs = overrideCells w x y [(addLife (head carns) (100 * (length herbs)))]
  | length herbs == 1 && 0 < length plants =  overrideCells w x y [(addLife (head herbs) (100 * (length plants)))]
  | otherwise = w
    where
      cells = getCell w x y
      plants = pickType cells Plant
      herbs = pickType cells Herbivore
      carns = pickType cells Carnivore

removeDeadCell :: World -> World
removeDeadCell w = foldl removeDead w xys

removeDead :: World -> (Int, Int) -> World
removeDead w (x, y) = foldl iter w (getCell w x y)
     where
       iter res c@(Cell _ int) = if 0 < life int
         then res
         else replaceCell res x y c (emptyCell x y)

removeDuplicateCells :: World -> World
removeDuplicateCells w = removed xys w
  where
    removed [] res = res
    removed (_xy:_xys) res = removed _xys (removeDuplicateCell res _xy)

removeDuplicateCell :: World -> (Int, Int) -> World
removeDuplicateCell w (x, y) = overrideCell w x y newCell
  where
    newCell = removeDuplicate cells x y
    cells = getCell w x y

removeDuplicate :: [Cell] -> Int -> Int -> Cell
removeDuplicate [] x y = emptyCell x y
removeDuplicate cs x y = if length cs == 1
  then (head cs)
  else pickStrongest x y cs

pickStrongest :: Int -> Int -> [Cell] -> Cell
pickStrongest x y cs
  | containsType cs Carnivore = resolve cs Carnivore x y
  | containsType cs Herbivore =  resolve cs Herbivore x y
  | containsType cs Plant = resolve cs Plant x y
  | otherwise = emptyCell x y
  
containsType :: [Cell] -> Type -> Bool
containsType cs t = 0 < (length $ pickType cs t)

pickType :: [Cell] -> Type -> [Cell]
pickType cs t = filter (\(Cell ext _) -> cellType ext == t) cs

resolve :: [Cell] -> Type -> Int -> Int -> Cell
resolve cs t x y = let ls = pickType cs t
  in case length ls of
    1 -> (head ls)
    _ -> emptyCell x y
