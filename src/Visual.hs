module Visual
    ( displayLines
    ) where

import CustomTypes
import Data.List(sortBy)

{-
Library for visualizing file information provided by a directory traversal
-}

displayLines :: [FileInfo] -> String -> [String]
displayLines infoList prefix = strList
  where trimmedList = [x | x <- infoList]
        sortedList = sortBy (\x y -> compare (show (fPath x)) (show (fPath y))) trimmedList
        strList = [(getElegantOutput x prefix) | x <- sortedList]

getElegantOutput :: FileInfo -> String -> String
getElegantOutput info prefix = (getPrefix info (depth info) (depth info)) ++ (rmPrefix (prefix) (fPath info)) ++ " " ++ (show $ fSize (stats info))

getPrefix :: FileInfo -> Int -> Int -> String
getPrefix info currDepth initDepth
  | currDepth == 0 = " "
  | currDepth == initDepth = (getPrefix info (currDepth - 1) initDepth) ++ "\\_"
  | otherwise = (getPrefix info (currDepth - 1) initDepth) ++ "    "

-- https://stackoverflow.com/questions/18554083/haskell-program-to-remove-part-of-list-and-print-the-rest
rmPrefix :: Eq a => [a] -> [a] -> [a]
rmPrefix [] ys = ys
rmPrefix _ [] = []
rmPrefix xs ys =
  if xs == take n ys
  then drop n ys
  else ys
  where n = length xs