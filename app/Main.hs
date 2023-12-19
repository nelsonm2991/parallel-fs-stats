module Main (main) where

import Lib
import Traversal

main :: IO ()
main = do
  list <- traverseFS "/home" []
  return ()
