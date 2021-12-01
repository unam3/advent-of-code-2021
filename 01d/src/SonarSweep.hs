module SonarSweep where

parseInput :: String -> [Int]
parseInput = fmap read . lines

makeListOfTuples :: [Int] -> [(Int, Int)]
makeListOfTuples list@(_:_:_) =
    let listTail = tail list
    in zip list listTail
makeListOfTuples _ = []


makeListOfTriplets :: [Int] -> [(Int, Int, Int)]
makeListOfTriplets list@(_:_:_:_) = 
    let listTail = tail list
        listTail1 = tail listTail
    in zip3 list listTail listTail1
makeListOfTriplets _ = []

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

