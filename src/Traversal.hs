module Traversal
    ( someOtherFunc
    ) where

import System.Posix.Files
import System.Posix.Types

someOtherFunc :: IO ()
someOtherFunc = putStrLn "someOtherFunc"

{-
Library for traversing the file system to collect [[FilePath : FileStats]]
adjacency lists
-}