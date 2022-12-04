import System.Environment
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.List
import Data.Maybe

data Section = Section Int Int deriving (Show, Eq)
data Pair = Pair Section Section deriving (Show, Eq)


testInput = "2-4,6-8\n\
  \2-3,4-5\n\
  \5-7,7-9\n\
  \2-8,3-7\n\
  \6-6,4-6\n\
  \2-6,4-8\n"


stringPairToSection :: [Text.Text] -> Section
stringPairToSection (a:b:_) = (Section (read a' ::Int) (read b' :: Int))
  where
    a' = Text.unpack a
    b' = Text.unpack b

stringToPair :: String -> Pair
stringToPair str = (Pair (sections !! 0) (sections !! 1))
  where
    pairAsList = Text.split (==',') (Text.pack str)
    splitOnDash = Text.split (=='-')
    sectionList = map splitOnDash pairAsList
    sections = map stringPairToSection sectionList


fullyOverlaps :: Pair -> Bool
fullyOverlaps (Pair (Section oneStart oneEnd) (Section twoStart twoEnd))
  | (oneStart <= twoStart) && (oneEnd >= twoEnd) = True
  | (twoStart <= oneStart) && (twoEnd >= oneEnd) = True
  | otherwise = False

numPairsWithFullOverlap :: [Pair] -> Int
numPairsWithFullOverlap pairs = length (filter fullyOverlaps pairs)

solve01 :: String -> Int
solve01 input = numPairsWithFullOverlap (map stringToPair (lines input))

solve01FromFile = do
  input <- readFile "04input.txt"
  putStrLn $ show (solve01 input)

overlaps :: Pair -> Bool
overlaps (Pair (Section oneStart oneEnd) (Section twoStart twoEnd))
  | (oneStart <= twoStart) && (twoStart <= oneEnd) = True
  | (twoStart <= oneStart) && (oneStart <= twoEnd) = True
  | otherwise = False


numPairsWithOverlap :: [Pair] -> Int
numPairsWithOverlap pairs = length (filter overlaps pairs)

solve02 :: String -> Int
solve02 input = numPairsWithOverlap (map stringToPair (lines input))

solve02FromFile = do
  input <- readFile "04input.txt"
  putStrLn $ show (solve02 input)