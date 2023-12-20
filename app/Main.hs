module Main (main) where

import Lib
import Traversal
import TraversalPar
import Data.List(sortBy)

main :: IO ()
main = do
  list <- traverseFS 0 "/Users/matthew/Desktop"
  --list <- traverseFSPar 0 "/Users/matthew/Desktop"
  -- Issue: we have an error when calling traverseFS that we are failing to deal with
  --putStrLn "hello"
  -- sortBy (\(a,_) (b,_) -> compare a b) [(2, "world"), (4, "!"), (1, "Hello")]
  --let sortedList = sortBy (\a b -> compare (fPath a) (fPath b)) list
  mapM_ (putStrLn . show) list

  return ()
