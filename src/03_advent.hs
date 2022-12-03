import System.Environment
import Data.Text ()
import qualified Data.Text.IO as TIO
import qualified Data.Set as Set
import Data.List
import Data.Maybe

testInput = "vJrwpWtwJgWrhcsFMMfFFhFp\n\
  \jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
  \PmmdzqPrVvPwwTWBwg\n\
  \wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
  \ttgJtRGJQctTZtZT\n\
  \CrZsJsPPZsGzwwsLwLmpwMDw\n"



itemChars = ['a'..'z'] ++ ['A'..'Z']

rucksackToCompartments :: String -> (Set.Set Char, Set.Set Char)
rucksackToCompartments r = (Set.fromList comp1, Set.fromList comp2)
  where len = length r
        (comp1, comp2) = splitAt (len `div` 2) r

findDuplicate :: (Set.Set Char, Set.Set Char) -> Char
findDuplicate (comp1, comp2) = head (Set.toList (Set.intersection comp1 comp2))

itemPriority :: Char -> Int
itemPriority x = 1 + (fromJust (elemIndex x itemChars))

rucksackDuplicatePriority :: String -> Int
rucksackDuplicatePriority rucksack = itemPriority (findDuplicate (rucksackToCompartments rucksack))

solve01 :: String -> Int
solve01 input = sum (map rucksackDuplicatePriority (lines input))

solve01FromFile = do
  input <- readFile "03input.txt"
  putStrLn $ show (solve01 input)


listByThrees :: [a] -> [(a, a, a)]
listByThrees [] = []
listByThrees (x:y:z:xs) = [(x, y, z)] ++ listByThrees xs

findBadge :: (String, String, String) -> Char
findBadge (x, y, z) = head (Set.toList (Set.intersection (Set.intersection (Set.fromList x) (Set.fromList y)) (Set.fromList z)))

solve02 :: String -> Int
solve02 input = sum (map (itemPriority . findBadge) (listByThrees (lines input)))

solve02FromFile = do
  input <- readFile "03input.txt"
  putStrLn $ show (solve02 input)