{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Void

import Data.Tree
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text.Text

-- To test parser in ghci
-- :set -XOverloadedStrings
-- parseTest ParserNameHere "string input"




-- Problem data types

data CDTarget = Root | Up | String deriving (Show, Eq)
data Operation = CD CDTarget | LS deriving (Show, Eq)
data File = File String Integer deriving (Show, Eq, Ord)
data Dir = Dir String  (Maybe Dir) [Dir] [File] deriving (Show, Eq, Ord)
-- data FSEntry = File String Integer | Dir String [FSEntry] deriving (Show, Eq)

getFileSize :: File -> Integer
getFileSize (File _ size) = size

getDirSize :: Dir -> Integer
getDirSize (Dir parent name [] []) = 0
getDirSize (Dir parent name [] files) = sum (map getFileSize files)
getDirSize (Dir parent name subdirs []) = sum (map getDirSize subdirs)
getDirSize (Dir parent name subdirs files) = sum (map getFileSize files) + sum (map getDirSize subdirs)

getDirSizes :: Map.Map Dir Integer -> Dir -> Map.Map Dir Integer
getDirSizes sizesByDir dir@(Dir parent name [] []) = Map.insert (dir) 0 sizesByDir
getDirSizes sizesByDir dir@(Dir parent name [] files) = Map.insert (dir) fileSizes sizesByDir
  where fileSizes = (sum (map getFileSize files))
getDirSizes sizesByDir dir@(Dir parent name subdirs files) = Map.insert (dir) (dirSizes + fileSizes) updatedSizesByDir
  where fileSizes = (sum (map getFileSize files))
        updatedSizesByDir = Map.unions ([sizesByDir] ++ (map (getDirSizes Map.empty) subdirs))
        dirSizes = sum [fromJust (Map.lookup subdir updatedSizesByDir) | subdir <- subdirs]
-- getDirSizes (sizesByDir, dir@(Dir parent name (subdir:xs) files)) = Map.insert (dir) (dirSize + fileSizes) sizesByDir
--   where fileSizes = (sum (map getFileSize files))
--         updatedSizesByDir = getDirSizes(sizesByDir, subdir)
--         dirSize = Map.lookup subdir updatedSizesByDir
-- -- getDirSizes (sizesByDir, dir@(Dir parent name subdirs files)) = Map.insert (dir)  (fileSize + dirSize) sizesByDir
--   where
--     fileSize = sum (map getFileSize files)
--     -- dirMap = getDirSizes (sizesByDir, ) -- Need to run getDirSizes on each subdir, and combine the resulting map with the map from the previous call
--     -- probably something like reduce or foldl?
--     dirMap = foldl (getDirSizes) sizesByDir subdirs
--     dirSize = sum [Map.lookup d dirMap | d <- subdirs]


testEmptyDir = Dir "/" Nothing [] []
testFilesOnlyDir = Dir "/" Nothing [] [File "a" 11, File "b" 22]
testDirsOnlyDir = Dir "/" Nothing [testFilesOnlyDir] []
testDir = Dir "/" Nothing [Dir "stuff" Nothing [Dir "empty" Nothing [] []] [File "a" 11, File "d" 44]] [File "b" 22, File "c" 33]