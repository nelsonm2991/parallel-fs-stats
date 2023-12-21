module Main (main) where

import Traversal
import TraversalPar
import TraversalParV2
import Visual
import System.Environment(getArgs)

-- "/home/matthew/PFP"

-- Argument structure:
-- <traversal type number> <absolute path of root directory>
main :: IO ()
main = do
  (choice:rootDir:_) <- getArgs
  case choice of
    "0" -> do
      -- Sequential traversal
      list <- traverseFS 0 rootDir
      mapM_ putStrLn (displayLines list rootDir)
    "1" -> do
      -- "Parallel" traversal, V1
      list <- traverseFSPar 0 rootDir
      mapM_ putStrLn (displayLines list rootDir)
    "2" -> do
      -- Parallel traversal, V2
      list <- traverseFSParV2 0 rootDir
      mapM_ putStrLn (displayLines list rootDir)
    _ -> do
      list <- traverseFSParV2 0 rootDir
      mapM_ putStrLn (displayLines list rootDir)

  return ()
