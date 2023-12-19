module Traversal
    ( traverseFS
    ) where

import System.Directory
import System.Posix.Files
import System.Posix.Types
import Control.Monad(forM, guard)

data FileInfo = FileInfo { fPath :: FilePath
                         , stats :: FileStats }

data FileStats = FileStats { devID :: DeviceID
                           , fSize :: FileOffset
                           , isDir :: Bool }

{-
Library for traversing the file system to collect [[FilePath : FileStats]]
adjacency lists
-}

traverseFS :: FilePath -> IO [FileInfo]
traverseFS path = do
  putStrLn ("considering: " ++ path)
  exists <- doesDirectoryExist path
  case exists of
    True -> do
      -- Get the contents of the directory
      dirContents <- listDirectory path

      -- Convert paths to file information, and get seperate list for sub-directories
      let filePaths = map (\fileName -> path ++ "/" ++ fileName) dirContents
      fileInfos <- getFileInfos filePaths
      let subDirsInfos = [subDir | subDir <- fileInfos, (isDir (stats subDir))]
      let subDirPaths = [(fPath subDir) | subDir <- subDirsInfos]

      -- Recursively explore the sub-directories and grab their [FileInfo]s
      overallResult <- concatMapM (traverseFS) subDirPaths

      return (fileInfos ++ overallResult)
    False -> do
      -- FileInfo for this file already collected via parent directory, so ignore it
      return []

getFileInfos :: [FilePath] -> IO [FileInfo]
getFileInfos filePaths = forM filePaths $ \filePath -> do
  fileStatus <- getFileStatus filePath
  let toFileInfo :: FilePath -> FileStatus -> FileInfo
      toFileInfo filePath fileStatus = FileInfo { fPath = filePath, stats = FileStats { devID = (deviceID fileStatus)
                                                                                     , fSize = (fileSize fileStatus)
                                                                                     , isDir = (isDirectory fileStatus)}}
  return (toFileInfo filePath fileStatus)

-- Taken from: https://hackage.haskell.org/package/extra-1.7.14/docs/src/Control.Monad.Extra.html#concatMapM
-- Had issues importing it
-- | A version of 'concatMap' that works with a monadic predicate.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
{-# INLINE concatMapM #-}
concatMapM op = foldr f (pure [])
    where f x xs = do x <- op x; if null x then xs else do xs <- xs; pure $ x++xs