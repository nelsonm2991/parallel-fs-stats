module Main (main) where

import Lib
import Traversal

main :: IO ()
main = do
  --putStrLn "before hello"
  list <- traverseFS 0 "/Users/matthew/PFP/fs-project"
  -- Issue: we have an error when calling traverseFS that we are failing to deal with
  --putStrLn "hello"
  mapM_ (putStrLn . show) list

  return ()
