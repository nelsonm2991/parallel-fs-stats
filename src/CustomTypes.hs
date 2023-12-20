module CustomTypes
    ( FileInfo(..)
    , FileStats(..)
    , getFileInfos
    , concatMapM
    ) where

-- chage

import System.Posix.Files
import System.Posix.Types
import Control.Monad(forM)
import Data.List(intercalate)

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

getFileInfos :: [FilePath] -> Int -> IO [FileInfo]
getFileInfos filePaths fileDepth = forM filePaths $ \indivFilePath -> do
  indivFileStatus <- getFileStatus indivFilePath
  let toFileInfo :: FilePath -> FileStatus -> FileInfo
      toFileInfo filePath fileStatus = FileInfo { fPath = filePath
                                                , stats = FileStats { devID = (deviceID fileStatus)
                                                                    , fSize = (fileSize fileStatus)
                                                                    , isDir = (isDirectory fileStatus)}
                                                , depth = fileDepth}
  return (toFileInfo indivFilePath indivFileStatus)

-- Taken from: https://hackage.haskell.org/package/extra-1.7.14/docs/src/Control.Monad.Extra.html#concatMapM
-- Had issues importing it
-- | A version of 'concatMap' that works with a monadic predicate.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
{-# INLINE concatMapM #-}
concatMapM op = foldr f (pure [])
    where f x xs = do xRes <- op x; if null xRes then xs else do xsRes <- xs; pure $ xRes++xsRes