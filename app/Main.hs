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
  -- Args
  -- 0 -> sequential traversal
  -- 1 -> parallel traversal, version 1
  -- 2 -> parallel traversal, version 2
  case choice of
    "0" -> do
      list <- traverseFS 0 rootDir
      mapM_ putStrLn (displayLines list rootDir)
    "1" -> do
      list <- traverseFSPar 0 rootDir
      mapM_ putStrLn (displayLines list rootDir)
    "2" -> do
      list <- traverseFSParV2 0 rootDir
      mapM_ putStrLn (displayLines list rootDir)
    _ -> do
      list <- traverseFSParV2 0 rootDir
      mapM_ putStrLn (displayLines list rootDir)

  return ()
