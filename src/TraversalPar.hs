module TraversalPar
    ( traverseFSPar
    ) where

import System.Directory
import System.Posix.Files
import System.Posix.Types
import Control.Monad(forM, guard)
import Data.List(intercalate)
import Control.Parallel.Strategies(rpar, parMap)

data FileInfo = FileInfo { fPath :: FilePath
                         , stats :: FileStats
                         , depth :: Int }

data FileStats = FileStats { devID :: DeviceID
                           , fSize :: FileOffset
                           , isDir :: Bool }

printFields :: [a -> String] -> a -> String
printFields functs object = intercalate " " $ map ($ object) functs

instance Show FileStats where
  show = printFields [show . devID, show . fSize, show . isDir]

instance Show FileInfo where
  show = printFields [show . fPath, show . stats, show . depth]

-- TODO: Consider defining Eq for both types so we can sort lists of them efficiently

{-
Library for traversing the file system to collect [[FilePath : FileStats]]
adjacency lists
-}

traverseFSPar :: Int -> FilePath -> IO [FileInfo]
traverseFSPar depth path = do
  exists <- doesDirectoryExist path
  case exists of
    True -> do
      -- Get the contents of the directory
      dirContents <- listDirectory path

      -- Convert paths to file information, and get seperate list for sub-directories
      let filePaths = map (\fileName -> path ++ "/" ++ fileName) dirContents
      fileInfos <- getFileInfos filePaths depth
      let subDirsInfos = [subDir | subDir <- fileInfos, (isDir (stats subDir))]
      let subDirPaths = [(fPath subDir) | subDir <- subDirsInfos]

      -- Recursively explore the sub-directories and grab their [FileInfo]s
      --overallResult <- concatMapM (traverseFSPar (depth + 1)) subDirPaths
      --overallResult <- parMap rpar (traverseFSPar (depth + 1)) subDirPaths
      --let overallResultLists = parMap rpar (traverseFSPar (depth + 1)) subDirPaths
      --overallResult <- consolIOLists overallResultLists

      -- Depth limit on parallelization
      case (depth < 4) of
        True -> do
          let overallResultLists = parMap rpar (traverseFSPar (depth + 1)) subDirPaths
          overallResult <- consolIOLists overallResultLists
          return (fileInfos ++ overallResult)
        False -> do
          overallResult <- concatMapM (traverseFSPar (depth + 1)) subDirPaths
          return (fileInfos ++ overallResult)


      --return (fileInfos ++ overallResult)
    False -> do
      -- FileInfo for this file already collected via parent directory, so ignore it
      return []

getFileInfos :: [FilePath] -> Int -> IO [FileInfo]
getFileInfos filePaths fileDepth = forM filePaths $ \filePath -> do
  fileStatus <- getFileStatus filePath
  let toFileInfo :: FilePath -> FileStatus -> FileInfo
      toFileInfo filePath fileStatus = FileInfo { fPath = filePath
                                                , stats = FileStats { devID = (deviceID fileStatus)
                                                                    , fSize = (fileSize fileStatus)
                                                                    , isDir = (isDirectory fileStatus)}
                                                , depth = fileDepth}
  return (toFileInfo filePath fileStatus)

-- Taken from: https://hackage.haskell.org/package/extra-1.7.14/docs/src/Control.Monad.Extra.html#concatMapM
-- Had issues importing it
-- | A version of 'concatMap' that works with a monadic predicate.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
{-# INLINE concatMapM #-}
concatMapM op = foldr f (pure [])
    where f x xs = do x <- op x; if null x then xs else do xs <- xs; pure $ x++xs

consolIOLists :: [IO [a]] -> IO [a]
consolIOLists lists = do
  allLists <- sequence lists
  return (concat allLists)