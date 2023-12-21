module TraversalPar
    ( traverseFSPar
    ) where

import CustomTypes
import System.Directory
import Control.Parallel.Strategies(rpar, parMap)

{-
Library for traversing the file system to collect [FileInfo]
for all encoutnered files and directories, in a not-so-parallel fashion
-}

traverseFSPar :: Int -> FilePath -> IO [FileInfo]
traverseFSPar depthVal path = do
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

      -- Parallel traverse if above depth of 3 (0,1,2), sequential otherwise
      case (depthVal < 3) of
        True -> do
          let overallResultLists = parMap rpar (traverseFSPar (depthVal + 1)) subDirPaths
          overallResult <- consolIOLists overallResultLists
          return (fileInfos ++ overallResult)
        False -> do
          overallResult <- concatMapM (traverseFSPar (depthVal + 1)) subDirPaths
          return (fileInfos ++ overallResult)

    False -> do
      -- FileInfo for this file already collected via parent directory, so ignore it
      return []

consolIOLists :: [IO [a]] -> IO [a]
consolIOLists lists = do
  allLists <- sequence lists
  return (concat allLists)
