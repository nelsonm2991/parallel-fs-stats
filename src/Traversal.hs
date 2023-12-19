module Traversal
    ( traverseFS
    ) where

import System.Directory
import System.Posix.Files
import System.Posix.Types

data FileInfo = FileAllStats { path :: FilePath
                                 , stats :: FileStats }

data FileStats = FileStats { deviceID :: DeviceID
                           , fileSize :: FileOffset
                           , isDir :: Bool }

-- Todo: add instance Show for FileStats for easy output

{-
Library for traversing the file system to collect [[FilePath : FileStats]]
adjacency lists
-}

-- Traverse the file system to the given depth. Collection [FilePath : FilesStats]
-- mapping and append them to the final returned FilePath

traverseFS :: FilePath -> [FileInfo] -> IO [FileInfo]
traverseFS path infoList = do
  exists <- doesDirectoryExist path
  case exists of
    True -> do
      -- Get the contents of the directory
      --dirContents <- listDirectory path
      -- For now, try to output the contents of each path encountered
      --putStrLn (dirContents)
      putStrLn "hello"
      return []
    False -> return []



{-
readTableFile :: String -> (Handle -> IO a) -> IO [a]
readTableFile filename function = withFile filename ReadMode $ \mainHandle -> do
    let readContents :: Handle -> (Handle -> IO a) -> IO [a]
        readContents handle func = do
            eof <- hIsEOF handle
            case eof of
                True -> return []
                False -> do
                    currLine <- func handle
                    remaining <- readContents handle func
                    return (currLine : remaining)
    vals <- readContents mainHandle function
    return vals
-}