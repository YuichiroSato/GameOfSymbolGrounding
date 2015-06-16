import Test.HUnit
import System.IO
import DataTest.CellTest (cellTests)
import DataTest.WorldTest (worldTests)

main :: IO ()
main = do
  runTestText (putTextToHandle stderr False) cellTests
  runTestText (putTextToHandle stderr False) worldTests
  return ()
