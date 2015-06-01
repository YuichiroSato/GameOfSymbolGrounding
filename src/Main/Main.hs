import Main.Setting
import Graphics.UI.GLUT
import qualified Graphics.View as View
import qualified Data.World as World
import Data.IORef
import System.Random

timeInterval :: Int
timeInterval = 1000

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialWindowSize $= Size wSize wSize
  world <- newIORef $ World.initialize $ mkStdGen 12
  createWindow "Game of Symbol Grounding"
  displayCallback $= display world
  addTimerCallback timeInterval $ timer $ display world
  mainLoop

display :: IORef World.World -> DisplayCallback
display w = do
  modifyIORef w World.update
  world <- readIORef w
  View.render world
  
timer :: DisplayCallback -> IO ()
timer act = do
  act
  addTimerCallback timeInterval $ timer act
