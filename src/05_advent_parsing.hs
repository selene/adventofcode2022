{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.List
import Data.Maybe
import Data.Void

-- import Data.Attoparsec.ByteString.Char8
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text.Text

-- number to move, from stack, to stack
data Move = Move Int Int Int deriving (Show, Eq)
data Stacks = Stacks [[Char]] deriving (Show, Eq)

-- :set -XOverloadedStrings
-- Run with parseOnly moveParser "str"


testMoveParser :: Parser (Integer, Integer, Integer)
testMoveParser = do
  string "move "
  num <- some (digitChar)
  string " from "
  from <- some (digitChar)
  string " to "
  to <- some (digitChar)
  return $ ((read num ::Integer), (read from ::Integer), (read to ::Integer))


-- moveParser :: Parser Move
-- moveParser = do
--   string "move"
--   num <- decimal
--   string " from "
--   from <- decimal
--   string " to "
--   to <- decimal
--   return $ Move num (from-1) (to-1)

-- stackParser :: Parser [Char]
-- stackParser = do
--   decimal
--   char ' '
--   crates <- many' letter_ascii
--   return crates

-- stacksParser :: Parser Stacks
-- stacksParser = do
--   stacks <- many' stackParser <* endOfLine
--   return (Stacks stacks)


-- movesParser :: Parser [Move]
-- movesParser = many' $ moveParser <* endOfLine

testCrates = [['N', 'Z']
  , ['D', 'C', 'M']
  , ['P']
  ]

testMoves = [Move 1 1 0
  , Move 3 0 2
  , Move 2 1 0
  , Move 1 0 1
  ]

testCrateInput = "1 NZ\n\
  \2 DCM\n\
  \3 P\n"

testMoveInput = "move 1 from 2 to 1\n\
  \move 3 from 1 to 3\n\
  \move 2 from 2 to 1\n\
  \move 1 from 1 to 2\n"


removeOne :: ([[Char]], Int) -> ([[Char]], Char)
removeOne (stacks, idx) = (([
  if i == idx
    then tail (stacks !! i)
    else stacks !! i
  | i <- [0..((length stacks)-1)]
  ]), crate)
  where (crate:_) = stacks !! idx


addOne :: ([[Char]], Int, Char) -> ([[Char]])
addOne (stacks, idx, crate) = [
  if i == idx
    then [crate] ++ (stacks !! i)
    else stacks !! i
  | i <- [0..((length stacks)-1)]
  ]

moveOne :: ([[Char]], Int, Int) -> [[Char]]
moveOne (stacks, from, to) = addOne (removed, to, crate)
  where (removed, crate) = removeOne (stacks, from)

doMove :: ([[Char]], Move) -> ([[Char]])
doMove (stacks, (Move 1 from to)) = moveOne (stacks, from, to)
doMove (stacks, (Move count from to)) = doMove (
  (moveOne (stacks, from, to))
  , (Move (count-1) from to))

moveCrates :: ([[Char]], [Move]) -> [[Char]]
moveCrates (stacks, []) = stacks
moveCrates (stacks, (move:rest)) = moveCrates ((doMove (stacks, move)), rest)

solveMoveCrates :: ([[Char]], [Move]) -> [Char]
solveMoveCrates (crates, moves) = map head stacks
  where stacks = moveCrates (crates, moves)