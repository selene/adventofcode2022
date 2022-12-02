import qualified Data.Map.Strict as Map

inputsToShapes :: Char -> String
inputsToShapes 'A' = "Rock"
inputsToShapes 'B' = "Paper"
inputsToShapes 'C' = "Scissors"
inputsToShapes 'X' = "Rock"
inputsToShapes 'Y' = "Paper"
inputsToShapes 'Z' = "Scissors"
inputsToShapes x = "ERROR!!"


shapesToScores :: String -> Int
shapesToScores "Rock" = 1
shapesToScores "Paper" = 2
shapesToScores "Scissors" = 3
shapesToScores x = 0

outcomesToScores :: String -> Int
outcomesToScores "win" = 6
outcomesToScores "draw" = 3
outcomesToScores "lose" = 0
outcomesToScores x = 0

shapeToWinner :: String -> String
shapeToWinner "Rock" = "Paper"
shapeToWinner "Paper" = "Scissors"
shapeToWinner "Scissors" = "Rock"


playToShapes :: (Char, Char) -> (String, String)
playToShapes (them, you) = (inputsToShapes them, inputsToShapes you)

shapesToOutcome :: (String, String) -> String
shapesToOutcome (them, you) 
    | you == (shapeToWinner them) = "win"
    | you == them = "draw"
    | otherwise = "lose"

playToScore :: (Char, Char) -> Int
playToScore (themRaw, youRaw) = (outcomesToScores outcome) + (shapesToScores you)
    where (them, you) = playToShapes (themRaw, youRaw)
          outcome = shapesToOutcome (them, you)


solve01 input = sum (map playToScore input)

testInput = [('A', 'Y')
    , ('B', 'X')
    , ('C', 'Z') ]
