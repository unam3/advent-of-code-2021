module SonarSweep where

parseInput :: String -> [Int]
parseInput = fmap read . lines

makeListOfTuples :: [Int] -> [(Int, Int)]
makeListOfTuples [] = []
makeListOfTuples list =
    let listTail = tail list
    in zip list listTail

solveTest :: IO ()
solveTest = readFile "testInput"
    >>= print
        . length
        . filter (uncurry (<))
        . makeListOfTuples
        . parseInput

solveTest2 :: IO ()
solveTest2 = readFile "testInput"
    >>= print
        . parseInput

solve :: IO ()
solve = readFile "input.txt"
    >>= print
        . length
        . filter (uncurry (<))
        . makeListOfTuples
        . parseInput

solve2 :: IO ()
solve2 = readFile "input.txt"
    >>= print
        . parseInput

