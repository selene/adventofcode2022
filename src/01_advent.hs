import Data.List

testInput = [
    [ 1000
    , 2000
    , 3000 ]
    , [4000]
    , [5000, 6000]
    , [7000, 8000, 9000]
    , [10000] ]


calorieTotals x = map sum x

solve01 input = maximum (map sum input)
solve02 input = sum (take 3 (reverse (sort (map sum input))))