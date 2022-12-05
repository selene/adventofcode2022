{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.List
import Data.Maybe


-- number to move, from stack, to stack
data Move = Move Int Int Int deriving (Show, Eq)
data Stacks = Stacks [[Char]] deriving (Show, Eq)

-- -------------------------------------------------

testCrates = [['N', 'Z']
  , ['D', 'C', 'M']
  , ['P']
  ]

testMoves = [Move 1 1 0
  , Move 3 0 2
  , Move 2 1 0
  , Move 1 0 1
  ]

testCrateInput = "NZ\n\
  \DCM\n\
  \P\n"

testMoveInput = "1 2 1\n\
  \3 1 3\n\
  \2 2 1\n\
  \1 1 2\n"


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

moveFromString :: String -> Move
moveFromString str = (Move count from to)
  where numbers = Text.split (== ' ') (Text.pack str)
        count = read (Text.unpack (numbers !! 0)) :: Int
        from = (read (Text.unpack (numbers !! 1)) :: Int) - 1
        to = (read (Text.unpack (numbers !! 2)) :: Int) - 1

solve01 :: (String, String) -> String
solve01 (crateString, moveString) = solveMoveCrates (crates, moves)
  where crates = lines crateString
        moves = map moveFromString (lines moveString)

solve01FromFile = do
  crateString <- readFile "05input_crates.txt"
  moveString <- readFile "05input_moves.txt"
  putStrLn $ show (solve01 (crateString, moveString))


-- ---------- PART 02 ------------


removeCrates :: ([[Char]], Int, Int) -> ([[Char]], [Char])
removeCrates (stacks, num, from) = (([
  if i == from
    then drop num (stacks !! i)
    else stacks !! i
  | i <- [0..((length stacks)-1)]
  ]), removed)
  where removed = take num (stacks !! from)


addCrates :: ([[Char]], [Char], Int) -> ([[Char]])
addCrates (stacks, crates, to) = (([
  if i == to
    then crates ++ (stacks !! i)
    else stacks !! i
  | i <- [0..((length stacks)-1)]
  ]))

doMove2 :: ([[Char]], Move) -> ([[Char]])
doMove2 (stacks, (Move count from to)) = addCrates (afterRemove, removed, to)
  where (afterRemove, removed) = removeCrates (stacks, count, from)

moveCrates2 :: ([[Char]], [Move]) -> [[Char]]
moveCrates2 (stacks, []) = stacks
moveCrates2 (stacks, (move:rest)) = moveCrates2 ((doMove2 (stacks, move)), rest)

solveMoveCrates2 :: ([[Char]], [Move]) -> [Char]
solveMoveCrates2 (crates, moves) = map head stacks
  where stacks = moveCrates2 (crates, moves)

solve02 :: (String, String) -> String
solve02 (crateString, moveString) = solveMoveCrates2 (crates, moves)
  where crates = lines crateString
        moves = map moveFromString (lines moveString)

solve02FromFile = do
  crateString <- readFile "05input_crates.txt"
  moveString <- readFile "05input_moves.txt"
  putStrLn $ show (solve02 (crateString, moveString))