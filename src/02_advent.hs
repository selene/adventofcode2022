import qualified Data.Map.Strict as Map

inputToShape :: Char -> String
inputToShape 'A' = "Rock"
inputToShape 'B' = "Paper"
inputToShape 'C' = "Scissors"
inputToShape 'X' = "Rock"
inputToShape 'Y' = "Paper"
inputToShape 'Z' = "Scissors"
inputToShape x = "ERROR!!"

shapeToScore :: String -> Int
shapeToScore "Rock" = 1
shapeToScore "Paper" = 2
shapeToScore "Scissors" = 3
shapeToScore x = 0

outcomeToScore :: String -> Int
outcomeToScore "win" = 6
outcomeToScore "draw" = 3
outcomeToScore "lose" = 0
outcomeToScore x = 0

shapeToWinner :: String -> String
shapeToWinner "Rock" = "Paper"
shapeToWinner "Paper" = "Scissors"
shapeToWinner "Scissors" = "Rock"

shapeToLoser :: String -> String
shapeToLoser "Rock" = "Scissors"
shapeToLoser "Paper" = "Rock"
shapeToLoser "Scissors" = "Paper"


playToShapes :: (Char, Char) -> (String, String)
playToShapes (them, you) = (inputToShape them, inputToShape you)

shapesToOutcome :: (String, String) -> String
shapesToOutcome (them, you) 
    | you == (shapeToWinner them) = "win"
    | you == them = "draw"
    | otherwise = "lose"

playToScore :: (Char, Char) -> Int
playToScore (themRaw, youRaw) = (outcomeToScore outcome) + (shapeToScore you)
    where (them, you) = playToShapes (themRaw, youRaw)
          outcome = shapesToOutcome (them, you)


solve01 input = sum (map playToScore input)

testInput = [('A', 'Y')
    , ('B', 'X')
    , ('C', 'Z') ]



inputToOutcome :: Char -> String
inputToOutcome 'X' = "lose"
inputToOutcome 'Y' = "draw"
inputToOutcome 'Z' = "win"

playToShapeOutcome :: (Char, Char) -> (String, String)
playToShapeOutcome (them, you) = (inputToShape them, inputToOutcome you)

outcomeToShape :: (String, String) -> String
outcomeToShape (them, outcome)
    | outcome == "win" = shapeToWinner them
    | outcome == "draw" = them
    | outcome == "lose" = shapeToLoser them

playToScore2 :: (Char, Char) -> Int
playToScore2 (themRaw, youRaw) = (outcomeToScore outcome) + (shapeToScore you)
    where (them, outcome) = playToShapeOutcome (themRaw, youRaw)
          you = outcomeToShape (them, outcome)

solve02 input = sum (map playToScore2 input)
