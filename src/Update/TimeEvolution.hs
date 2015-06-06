module Update.TimeEvolution(evolve) where

import Data.Cell
import qualified Data.World as World
import qualified Data.List as List

type Mid = [((Int, Int), [Cell])]

midMap :: ([Cell] -> [Cell]) -> Mid -> Mid
midMap f m = map (\(t, cs) -> (t, f cs)) m

fromList2Mid :: [Cell] -> Mid
fromList2Mid cs = toId $ toTuple $ List.groupBy eq $ List.sortBy comp cs
  where
    comp (Cell a _) (Cell b _) = (xy a) `compare` (xy b)
    eq (Cell a _) (Cell b _) = (xy a) == (xy b)
    toTuple css = map (\ls -> (head ls, ls)) css
    toId css = map (\(Cell ext _, ls) -> (xy ext, ls)) css

evolve :: World.World -> World.World
evolve w = World.fromList m
  where
    m = removeDuplicate $ eatCell $ fromList2Mid (moved ++ newCells)
    moved = moveAnimal oldCells
    (oldCells, newCells) = makeBirth livingCells
    livingCells = removeDead $ payCost $ World.toCellList w

payCost :: [Cell] -> [Cell]
payCost cs = map consume cs

makeBirth :: [Cell] -> ([Cell], [Cell])
makeBirth cs = foldl hoge ([], []) $ map born2 cs
  where
    hoge (olds, news) (c, mc) = case mc of
      Just c2 -> (c:olds, c2:news)
      Nothing -> (c:olds, news)

moveAnimal :: [Cell] -> [Cell]
moveAnimal cs = map move cs

eatCell :: Mid -> Mid
eatCell m = midMap iter m
  where
    iter cs
      | length carns == 1 && 0 < length herbs = [(addLife (head carns) (100 * (length herbs)))]
      | length herbs == 1 && 0 < length plants = [(addLife (head herbs) (100 * (length plants)))]
      | otherwise = cs
        where
          plants = pickType cs Plant
          herbs = pickType cs Herbivore
          carns = pickType cs Carnivore
          pickType :: [Cell] -> Type -> [Cell]
          pickType css t = filter (\(Cell ext _) -> cellType ext == t) css

removeDead :: [Cell] -> [Cell]
removeDead cs = filter (\c -> not $ isDead c) cs

removeDuplicate :: Mid -> Mid
removeDuplicate m = midMap resolve m
  where
    resolve cs
      | length nonempty == 1 = nonempty
      | otherwise = empty
        where
          nonempty = filter (\c -> not $ isEmptyCell c) cs
          empty = filter (\c -> isEmptyCell c) cs
