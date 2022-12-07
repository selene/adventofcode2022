{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.List
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
data File = File String Integer deriving (Show, Eq)
data Dir = Dir String [Dir] [File] deriving (Show, Eq)
-- data FSEntry = File String Integer | Dir String [FSEntry] deriving (Show, Eq)

getFileSize :: File -> Integer
getFileSize (File _ size) = size

getDirSize :: Dir -> Integer
getDirSize (Dir name [] [])  = 0
getDirSize (Dir name [] files) = sum (map getFileSize files)
getDirSize (Dir name subdirs []) = sum (map getDirSize subdirs)
getDirSize (Dir name subdirs files) = sum (map getFileSize files) + sum (map getDirSize subdirs)