import System.Environment
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.List
import qualified Data.Set as Set
import Data.Maybe


testInput1 = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
testInput2 = "bvwbjplbgvbhsrlpgdmjqwftvncz"
testInput3 = "nppdvjthqldpwncqszvftbrmjlhg"
testInput4 = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
testInput5 = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"

noDuplicates :: String -> Bool
noDuplicates str = (length (Set.fromList str)) == (length str)

findMarkerEnd :: String -> Int -> Int-> Maybe Int
findMarkerEnd str end size
    | (length str) == size = if noDuplicates str
                          then Just end
                          else Nothing
    | otherwise = if noDuplicates (take size str)
                  then Just end
                  else findMarkerEnd (tail str) (end + 1) size

solve01 :: String -> Maybe Int
solve01 str = findMarkerEnd str 4 4

-- Tests

part1Test1 = let marker = solve01 testInput1 in (isJust marker) && (fromJust marker) == 7
part1Test2 = let marker = solve01 testInput2 in (isJust marker) && (fromJust marker) == 5
part1Test3 = let marker = solve01 testInput3 in (isJust marker) && (fromJust marker) == 6
part1Test4 = let marker = solve01 testInput4 in (isJust marker) && (fromJust marker) == 10
part1Test5 = let marker = solve01 testInput5 in (isJust marker) && (fromJust marker) == 11

solve01FromFile =  do
  input <- readFile "06input.txt"
  putStrLn $ show (solve01 input)

solve02 :: String -> Maybe Int
solve02 str = findMarkerEnd str 14 14

part2Test1 = let marker = solve02 testInput1 in (isJust marker) && (fromJust marker) == 19
part2Test2 = let marker = solve02 testInput2 in (isJust marker) && (fromJust marker) == 23
part2Test3 = let marker = solve02 testInput3 in (isJust marker) && (fromJust marker) == 23
part2Test4 = let marker = solve02 testInput4 in (isJust marker) && (fromJust marker) == 29
part2Test5 = let marker = solve02 testInput5 in (isJust marker) && (fromJust marker) == 26

solve02FromFile =  do
  input <- readFile "06input.txt"
  putStrLn $ show (solve02 input)