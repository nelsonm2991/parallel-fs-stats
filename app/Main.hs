module Main (main) where

import Lib
import Traversal
import Data.List(sortBy)

main :: IO ()
main = do
  --putStrLn "before hello"
  --list <- traverseFS 0 "/Users/matthew/PFP/fs-project"
  list <- traverseFS 0 "/Users/matthew/PFP/fs-project"
  -- Issue: we have an error when calling traverseFS that we are failing to deal with
  --putStrLn "hello"
  -- sortBy (\(a,_) (b,_) -> compare a b) [(2, "world"), (4, "!"), (1, "Hello")]
  let sortedList = sortBy (\a b -> compare (fPath a) (fPath b)) list
  mapM_ (putStrLn . show) sortedList

  return ()
