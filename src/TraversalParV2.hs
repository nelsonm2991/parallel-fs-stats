module TraversalParV2
    ( traverseFSParV2
    ) where

import CustomTypes
import System.Directory
import System.Posix.Files
import Control.Parallel.Strategies(rpar, parMap)
import Control.Concurrent.Async(forConcurrently)

{-
Library for traversing the file system to collect [FileInfo]
for all encoutnered files and directories, in a parallel fashion
-}

traverseFSParV2 :: Int -> FilePath -> IO [FileInfo]
traverseFSParV2 depthVal path = do
  exists <- doesDirectoryExist path
  case exists of
    True -> do
      -- Get the contents of the directory
      dirContents <- listDirectory path

      -- Convert paths to file information, and get seperate list for sub-directories
      let filePaths = map (\fileName -> path ++ "/" ++ fileName) dirContents
      fileInfos <- getFileInfosPar filePaths depthVal

      let subDirsInfos = [subDir | subDir <- fileInfos, (isDir (stats subDir))]
      let subDirPaths = [(fPath subDir) | subDir <- subDirsInfos]

      -- Parallel traverse if above depth of 3 (0,1,2), sequential otherwise
      case (depthVal < 3) of
        True -> do
          let overallResultLists = parMap rpar (traverseFSParV2 (depthVal + 1)) subDirPaths
          overallResult <- consolIOLists overallResultLists
          return (fileInfos ++ overallResult)
        False -> do
          overallResult <- concatMapM (traverseFSParV2 (depthVal + 1)) subDirPaths
          return (fileInfos ++ overallResult)

    False -> do
      -- FileInfo for this file already collected via parent directory, so ignore it
      return []

consolIOLists :: [IO [a]] -> IO [a]
consolIOLists lists = do
  allLists <- sequence lists
  return (concat allLists)

getFileInfosPar :: [FilePath] -> Int -> IO [FileInfo]
getFileInfosPar filePaths fileDepth = forConcurrently filePaths ( \indivFilePath -> do
  indivFileStatus <- getFileStatus indivFilePath
  let toFileInfo :: FilePath -> FileStatus -> FileInfo
      toFileInfo filePath fileStatus = FileInfo { fPath = filePath
                                                , stats = FileStats { devID = (deviceID fileStatus)
                                                                    , fSize = (fileSize fileStatus)
                                                                    , isDir = (isDirectory fileStatus)}
                                                , depth = fileDepth}
  return (toFileInfo indivFilePath indivFileStatus))