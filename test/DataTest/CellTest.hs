module DataTest.CellTest where

import Test.HUnit
import Data.Cell

car1 :: Cell
car1 = carnivore 0 0 1 2 2
car2 :: Cell
car2 = carnivore 2 2 1 2 2

her1 :: Cell
her1 = herbivore 1 1 1 2 2
her2 :: Cell
her2 = herbivore 1 1 1 3 2
her3 :: Cell
her3 = herbivore 3 3 1 2 2

pla1 :: Cell
pla1 = plant 1 1 1 1 1
pla2 :: Cell
pla2 = plant 1 1 1 1 1

empty1 :: Cell
empty1 = emptyCell 1 1

cellTests :: Test
cellTests = TestList (moveTests ++
                      consumeTests ++
                      eatTests ++
                      birthTests ++
                      resolveTests)

moveTests :: [Test]
moveTests =
  [ "move1" ~: (move car1) ~?= car2,
    "move2" ~: (move her1) ~?= her3,
    "move3" ~: (move pla1) ~?= pla2,
    "move4" ~: (move empty1) ~?= empty1
  ]

consumeTests :: [Test]
consumeTests =
  [ "consume1" ~: (consume car1) == car1 ~?= False 
  ]

eatTests :: [Test]
eatTests =
  [ "eat1" ~: length (eat [car1, her1]) ~?= 1
  ]

birthTests :: [Test]
birthTests =
  [ "birth1" ~: (birth car1) ~?= (car1, Nothing)
  ]

resolveTests :: [Test]
resolveTests =
  [ "resolve1" ~: (resolve [car1]) ~?= [car1],
    "resolve2" ~: (resolve [car1, car2]) ~?= [],
    "resolve3" ~: (resolve [her1]) ~?= [her1],
    "resolve4" ~: (resolve [car1, her1]) ~?= [],
    "resolve5" ~: (resolve [empty1]) ~?= []
  ]
