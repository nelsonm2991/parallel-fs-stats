module Main (main) where

--import CustomTypes
--import Traversal
import TraversalPar
import Visual
--import Data.List(sortBy)

main :: IO ()
main = do
  --list <- traverseFS 0 "/Users/matthew/PFP"
  -- line
  list <- traverseFSPar 0 "/home/matthew/PFP"
  mapM_ putStrLn (displayLines list "/home/matthew/PFP")

  return ()
