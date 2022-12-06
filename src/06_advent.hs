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

findMarkerEnd :: String -> Int -> Maybe Int
findMarkerEnd str end
    | (length str) == 4 = if noDuplicates str
                          then Just end
                          else Nothing
    | otherwise = if noDuplicates (take 4 str)
                  then Just end
                  else findMarkerEnd (tail str) (end + 1)

solve01 :: String -> Maybe Int
solve01 str = findMarkerEnd str 4

-- Tests

test1 = let marker = solve01 testInput1 in (isJust marker) && (fromJust marker) == 7
test2 = let marker = solve01 testInput2 in (isJust marker) && (fromJust marker) == 5
test3 = let marker = solve01 testInput3 in (isJust marker) && (fromJust marker) == 6
test4 = let marker = solve01 testInput4 in (isJust marker) && (fromJust marker) == 10
test5 = let marker = solve01 testInput5 in (isJust marker) && (fromJust marker) == 11

solve01FromFile =  do
  input <- readFile "06input.txt"
  putStrLn $ show (solve01 input)