module DataTest.WorldTest where

import Test.HUnit
import Data.Cell
import Data.World

car1 :: Cell
car1 = carnivore 0 0 1 2 2
her1 :: Cell
her1 = herbivore 1 1 1 2 2
pla1 :: Cell
pla1 = plant 1 1 1 0 0

world1 :: World
world1 = emptyWorld

world2 :: World
world2 = writeCell world1 0 0 car1

worldTests :: Test
worldTests = TestList (fromListTests ++
                      toListTests ++
                      toCellListTests ++
                      writeCellTests ++
                      readCellTests)

fromListTests :: [Test]
fromListTests =
  [ "fromList1" ~: fromList [((0,0), [car1])] ~?= world2
  ]

toListTests :: [Test]
toListTests =
  [ "toList1" ~: and (map (\(_, c) -> isEmptyCell c) (toList world1)) ~?= True
  ]

toCellListTests :: [Test]
toCellListTests =
  [ "toCellList1" ~: and (map isEmptyCell (toCellList world1)) ~?= True
  ]

writeCellTests :: [Test]
writeCellTests =
  [ "writeCell1" ~: (readCell world2 0 0) ~?= car1
  ]

readCellTests :: [Test]
readCellTests =
  [ "readCell1" ~: (readCell world1 0 0) ~?= (emptyCell 0 0) ]
