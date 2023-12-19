module Traversal
    ( traverseFS
    ) where

import System.Directory
import System.Posix.Files
import System.Posix.Types
import Control.Monad(forM)

data FileInfo = FileInfo { path :: FilePath
                         , stats :: FileStats }

data FileStats = FileStats { devID :: DeviceID
                           , fSize :: FileOffset
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
      dirContents <- listDirectory path
      let filePaths = map (\fileName -> path ++ "/" ++ fileName) dirContents
      fileInfos <- getFileInfos filePaths
      -- For now, try to output the contents of each path encountered
      --mapM_ putStrLn dirContents
      --putStrLn "hello"
      return []
    False -> return []

getFileInfos :: [FilePath] -> IO [FileInfo]
getFileInfos filePaths = forM filePaths $ \filePath -> do
  fileStatus <- getFileStatus filePath
  let toFileInfo :: FilePath -> FileStatus -> FileInfo
      toFileInfo filePath fileStatus = FileInfo { path = filePath, stats = FileStats { devID = (deviceID fileStatus)
                                                                                     , fSize = (fileSize fileStatus)
                                                                                     , isDir = (isDirectory fileStatus)}}
  return (toFileInfo filePath fileStatus)

{-
getDeviceIDs :: [FilePath] -> IO [DeviceID]
getDeviceIDs filePaths = forM filePaths $ \filePath -> do
  fileStatus <- getFileStatus filePath
  return $ fileDevice fileStatus
-}

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