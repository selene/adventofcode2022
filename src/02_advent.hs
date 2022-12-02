import qualified Data.Map.Strict as Map

data Shape = Rock | Paper | Scissors
    deriving (Eq, Ord, Show, Read, Bounded, Enum)
data Outcome = Win | Lose | Draw
    (Eq, Ord, Show, Read, Bounded, Enum)

inputsToShapes :: Char -> Shape
inputsToShapes 'A' = Rock
inputsToShapes 'B' = Paper
inputsToShapes 'C' = Scissors
inputsToShapes 'X' = Rock
inputsToShapes 'Y' = Paper
inputsToShapes 'Z' = Scissors

shapesToScores :: Shape -> Int
shapesToScores Rock = 1
shapesToScores Paper = 2
shapesToScores Scissors = 3

outcomesToScores :: Outcome -> Int
outcomesToScores Win = 6
outcomesToScores Draw = 3
outcomesToScores Lose = 0

shapeToWinner :: Shape -> Shape
shapeToWinner Rock = Paper
shapeToWinner Paper = Scissors
shapeToWinner Scissors = Rock


playToShapes :: (Char, Char) -> (Shape, Shape)
playToShapes (them, you) = (inputsToShapes them, inputsToShapes you)

shapesToOutcome :: (Shape, Shape) -> Outcome
shapesToOutcome (them, you) 
    | you == (shapeToWinner them) = Win
    | you == them = Draw
    | otherwise = Lose

playToScore :: (Char, Char) -> Int
playToScore (themRaw, youRaw) = (outcomesToScores outcome) + (shapesToScores you)
    where (them, you) = playToShapes (themRaw, youRaw)
          outcome = shapesToOutcome (them, you)


solve01 input = sum (map playToScore input)

testInput = [('A', 'Y')
    , ('B', 'X')
    , ('C', 'Z') ]



inputsToOutcomes :: Char -> String
inputsToOutcomes 'X' = "lose"
inputsToOutcomes 'Y' = "draw"
inputsToOutcomes 'Z' = "win"

-- playToShapeOutcome :: (Char, Char) -> (String, String)
-- playToShapeOutcome (them, you) = (inputsToShapes them, inputsToOutcomes you)

-- outcomeToShape :: (String, String) -> String
-- outcomeToShape (them, outcome)
--     | outcome == "win" = shapeToWinner them
--     | outcome == "draw" = them
--     | outcome == "lose" = them