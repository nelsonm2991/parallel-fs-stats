module TraversalParPremade
    ( traverseFSParPremade
    , FileInfo
    , FileStats
    , depth
    , stats
    , fPath
    , fSize
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

-- Use existing Haskell libraries to recursively, in parallel, traverse a file system
-- directory. Try to see how much improvement there is between the pre-made
-- parallel library and the one you (attempted to make) made
traverseFSParPremade :: Int -> FilePath -> IO [FileInfo]
traverseFSParPremade num path = do
  return []