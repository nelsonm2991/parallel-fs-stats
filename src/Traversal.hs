module Traversal
    ( traverseFS
    ) where

import CustomTypes
import System.Directory

{-
Library for traversing the file system to collect [FileInfo]
for all encoutnered files and directories
-}

traverseFS :: Int -> FilePath -> IO [FileInfo]
traverseFS depthVal path = do
  exists <- doesDirectoryExist path
  case exists of
    True -> do
      -- Get the contents of the directory
      dirContents <- listDirectory path

      -- Convert paths to file information, and get seperate list for sub-directories
      let filePaths = map (\fileName -> path ++ "/" ++ fileName) dirContents
      fileInfos <- getFileInfos filePaths depthVal
      let subDirsInfos = [subDir | subDir <- fileInfos, (isDir (stats subDir))]
      let subDirPaths = [(fPath subDir) | subDir <- subDirsInfos]

      -- Recursively explore the sub-directories and grab their [FileInfo]s
      overallResult <- concatMapM (traverseFS (depthVal + 1)) subDirPaths

      return (fileInfos ++ overallResult)
    False -> do
      -- FileInfo for this file already collected via parent directory, so ignore it
      return []
