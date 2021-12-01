module SonarSweep where

parseInput :: String -> [Int]
parseInput = fmap read . lines

makeListOfTuples :: [Int] -> [(Int, Int)]
makeListOfTuples [] = []
makeListOfTuples list =
    let listTail = tail list
    in zip list listTail


-- add guards for untotal tail
makeListOfTriplets :: [Int] -> [(Int, Int, Int)]
makeListOfTriplets [] = []
makeListOfTriplets list = 
    let listTail = tail list
        listTail1 = tail listTail
    in zip3 list listTail listTail1

sumTriplets :: (Int, Int, Int) -> Int
sumTriplets (m, m1, m2) = m + m1 + m2


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
        . length
        . filter (uncurry (<))
        . makeListOfTuples
        . fmap sumTriplets
        . makeListOfTriplets
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
        . length
        . filter (uncurry (<))
        . makeListOfTuples
        . fmap sumTriplets
        . makeListOfTriplets
        . parseInput

