import Main.Setting (wSize)
import Graphics.UI.GLUT
import Graphics.View (render)
import qualified Data.World as World
import Update.TimeEvolution (evolve)
import Data.IORef (IORef, newIORef, modifyIORef, readIORef)
import System.Random (getStdGen)

timeInterval :: Int
timeInterval = 100

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialWindowSize $= Size wSize wSize
  gen <- getStdGen
  world <- newIORef $ World.initialize gen
  createWindow "Game of Symbol Grounding"
  displayCallback $= display world
  addTimerCallback timeInterval $ timer $ display world
  mainLoop

display :: IORef World.World -> DisplayCallback
display w = do
  modifyIORef w evolve
  world <- readIORef w
  render world
  
timer :: DisplayCallback -> IO ()
timer act = do
  act
  addTimerCallback timeInterval $ timer act
