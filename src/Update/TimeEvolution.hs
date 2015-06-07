module Update.TimeEvolution(evolve) where

import Data.Cell
import Data.World (fromList, toCellList, World)
import Data.List (groupBy, foldl', sortBy)

type Mid = [((Int, Int), [Cell])]

midMap :: ([Cell] -> [Cell]) -> Mid -> Mid
midMap f m = map (\(t, cs) -> (t, f cs)) m

fromList2Mid :: [Cell] -> Mid
fromList2Mid cs0 = toXY $ toTuple $ groupBy eq $ sortBy comp cs0
  where
    comp (Cell a _) (Cell b _) = (xy a) `compare` (xy b)
    eq (Cell a _) (Cell b _) = (xy a) == (xy b)
    toTuple cs = map (\ls -> (head ls, ls)) cs
    toXY cs = map (\(Cell ext _, ls) -> (xy ext, ls)) cs

evolve :: World -> World
evolve w = fromList newWorld
  where
    newWorld = removeDuplicate $ eatCell $ fromList2Mid (moved ++ newCells)
    moved = moveAnimal oldCells
    (oldCells, newCells) = makeBirth livingCells
    livingCells = removeDead $ payCost $ toCellList w

payCost :: [Cell] -> [Cell]
payCost cs = map consume cs

makeBirth :: [Cell] -> ([Cell], [Cell])
makeBirth cs = foldl' collect ([], []) $ map birth cs
  where
    collect (olds, news) (c, mc) = case mc of
      Just c2 -> (c:olds, c2:news)
      Nothing -> (c:olds, news)

moveAnimal :: [Cell] -> [Cell]
moveAnimal cs = map move cs

eatCell :: Mid -> Mid
eatCell m = midMap eat m

removeDead :: [Cell] -> [Cell]
removeDead cs = filter (\c -> not $ isDead c) cs

removeDuplicate :: Mid -> Mid
removeDuplicate m = midMap resolve m
