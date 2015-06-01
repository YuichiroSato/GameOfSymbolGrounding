import Test.HUnit
import Data.Cell
import Data.World
import qualified Data.Vector as Vector
import Main.Setting
import System.IO


car00 = carnivoreCell 0 0 2 2
car01 = carnivoreCell 0 1 2 2
car02 = carnivoreCell 0 2 2 2
car03 = carnivoreCell 0 3 2 2
car04 = carnivoreCell 0 4 2 2
car05 = carnivoreCell 0 5 2 2

car10 = carnivoreCell 1 0 2 2
car11 = carnivoreCell 1 1 2 2
car11_2 = carnivoreCell 1 1 2 3
car12 = carnivoreCell 1 2 2 2
car13 = carnivoreCell 1 3 2 2
car14 = carnivoreCell 1 4 2 2
car15 = carnivoreCell 1 5 2 2

car20 = carnivoreCell 2 0 2 2
car21 = carnivoreCell 2 1 2 2
car22 = carnivoreCell 2 2 2 2
car23 = carnivoreCell 2 3 2 2
car24 = carnivoreCell 2 4 2 2
car25 = carnivoreCell 2 5 2 2

car33 = carnivoreCell 3 3 2 2
car34_2 = carnivoreCell 3 4 2 3

her11 = herbivoreCell 1 1 2 2
her11_2 = herbivoreCell 1 1 3 2
her33 = herbivoreCell 3 3 2 2

pla11 = plantCell 1 1
pla11_2 = plantCell 1 1

empty11 = emptyCell 1 1
empty33 = emptyCell 3 3

world1 :: World
world1 = overrideCell emptyWorld 1 1 car11

world2 :: World
world2 = overrideCell emptyWorld 3 3 car33

carxy = carnivoreCell (xSize - 2) (ySize - 2) 2 2

world3 :: World
world3 = overrideCell emptyWorld (xSize - 2) (ySize - 2) carxy

tests = TestList
  [ "carn " ~: (head (getCell (update world1) 3 3)) ~?= car33,
    "update " ~: world2 == update world1 ~?= True
  ]

cells1 = [car11, her11]
cells2 = [car11, car11_2]
cells3 = [car11, empty11]
cells4 = [her11, empty11]
cells5 = [her11, her11_2]
cells6 = [pla11, pla11_2]
cells7 = [pla11, empty11]
cells8 = [her11, pla11]
cells9 = [car11, her11, pla11]

publicTests = TestList (insert2ColomTest ++ insert2WorldTest)

smallColom1 = Colom $ Vector.fromList [[emptyCell 0 0], [emptyCell 1 0], [emptyCell 2 0]]
smallColom2 = Colom $ Vector.fromList [[emptyCell 0 1], [emptyCell 1 1], [emptyCell 2 1]]
smallColom3 = Colom $ Vector.fromList [[emptyCell 0 2], [emptyCell 1 2], [emptyCell 2 2]]

smallWorld1 = World $ Vector.fromList [smallColom1, smallColom2, smallColom2]

insert2ColomTest =
  [ "insert2Colom1" ~: (insert2Colom smallColom1 1 car01) ~?= (Colom $ Vector.fromList [[emptyCell 0 0], [car01, emptyCell 1 0], [emptyCell 2 0]])
  ]

insert2WorldTest =
  [ "insert2World1" ~: (getCell (insert2World smallWorld1 1 0 car10) 1 0) ~?= [car10, emptyCell 1 0]
  ]

privateTests = TestList (updateTest ++
                         moveTest ++
                         moveCellTest ++
                         removeDuplicateTest ++
                         pickStrongestTest ++
                         containsTypeTest ++
                         resolveTests ++
                         pickTypeTests)

updateTest =
  [ "update1" ~: (getCell (update world1) 1 1) ~?= [empty11], 
    "update2" ~: (getCell (update world1) 3 3) ~?= [car33],  
    "update3" ~: (update world1) ~?= world2  
  ]

moveTest =
  [ "move1" ~: (move car11) ~?= car33,
    "move2" ~: (move her11) ~?= her33,
    "move3" ~: (move pla11) ~?= pla11,
    "move4" ~: (move empty11) ~?= empty11,
    "move5" ~: (move car11_2) ~?= car34_2
  ]

moveCellTest =
  [ "moveCell1" ~: (getCell (moveCell world1 (1, 1) emptyWorld) 1 1) ~?= [empty11],
    "moveCell2" ~: (getCell (moveCell world1 (1, 1) emptyWorld) 3 3) ~?= [car33, empty33]
  ]

removeDuplicateTest =
  [ "removeDuplicate1" ~: (removeDuplicate cells1 1 1) ~?= car11,
    "removeDuplicate2" ~: (removeDuplicate cells2 1 1) ~?= empty11,
    "removeDuplicate3" ~: (removeDuplicate cells4 1 1) ~?= her11,
    "removeDuplicate4" ~: (removeDuplicate cells9 1 1) ~?= car11
  ]

pickStrongestTest =
  [ "pickStrongest1" ~: (pickStrongest 1 1 cells9) ~?= car11,
    "pickStrongest2" ~: (pickStrongest 1 1 cells3) ~?= car11,
    "pickStrongest3" ~: (pickStrongest 1 1 cells4) ~?= her11,
    "pickStrongest4" ~: (pickStrongest 1 1 cells7) ~?= pla11,
    "pickStrongest5" ~: (pickStrongest 1 1 cells2) ~?= empty11,
    "pickStrongest6" ~: (pickStrongest 1 1 cells5) ~?= empty11,
    "pickStrongest7" ~: (pickStrongest 1 1 cells6) ~?= empty11  
  ]

containsTypeTest =
  [ "containsType1" ~: (containsType cells9 Carnivore) ~?= True,
    "containsType2" ~: (containsType cells9 Herbivore) ~?= True,
    "containsType3" ~: (containsType cells9 Plant) ~?= True
  ]

pickTypeTests =
  [ "pickType1" ~: (pickType cells9 Carnivore) ~?= [car11],
    "pickType2" ~: (pickType cells9 Herbivore) ~?= [her11],
    "pickType3" ~: (pickType cells9 Plant) ~?= [pla11],
    "pickType4" ~: (pickType cells2 Carnivore) ~?= cells2,
    "pickType5" ~: (pickType cells5 Herbivore) ~?= cells5
  ]

resolveTests =
  [ "resolve1" ~: (resolve cells1 Carnivore 1 1) ~?= car11,
    "resolve2" ~: (resolve cells2 Carnivore 1 1) ~?= empty11,
    "resolve3" ~: (resolve cells3 Carnivore 1 1) ~?= car11,
    "resolve4" ~: (resolve cells4 Herbivore 1 1) ~?= her11,
    "resolve5" ~: (resolve cells5 Herbivore 1 1) ~?= empty11,
    "resolve6" ~: (resolve cells6 Plant 1 1) ~?= empty11,
    "resolve7" ~: (resolve cells7 Plant 1 1) ~?= pla11,
    "resolve8" ~: (resolve cells8 Herbivore 1 1) ~?= her11,
    "resolve9" ~: (resolve cells9 Carnivore 1 1) ~?= car11
  ]

main :: IO ()
main = do
  runTestText (putTextToHandle stderr False) publicTests
  runTestText (putTextToHandle stderr False) privateTests
  return ()
