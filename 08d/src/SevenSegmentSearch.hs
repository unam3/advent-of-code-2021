module SevenSegmentSearch where


import Data.List (foldl')

-- In the output values, how many times do digits 1, 4, 7, or 8 appear?

{-

1) Extract 4 digits after vertical bar "|"
2) count how many times one of four number (1,4,7,8) are occured in all exctracted lines

-}


removeBarSpace :: String -> String
removeBarSpace = drop 2

extractOtputValue :: String -> [String]
extractOtputValue = words . removeBarSpace . dropWhile (/= '|')

parseInput :: String -> [[String]]
parseInput = fmap extractOtputValue . lines

collectAppearenceOf1478 :: [String] -> [String]
collectAppearenceOf1478 =
    foldl'
        (\ acc digitRepresentation ->
            let len = length digitRepresentation
            in if len == 2 || len == 3 || len == 4 || len == 7
                then acc ++ [digitRepresentation]
                else acc
        )
        []

countAppearenceOf1478 :: [[String]] -> Int
countAppearenceOf1478 = sum . fmap (length . collectAppearenceOf1478)


solveTest :: IO ()
solveTest = readFile "testInput"
    >>= print
        . countAppearenceOf1478
        . parseInput

solve :: IO ()
solve = readFile "input.txt"
    >>= print
        . parseInput

solveTest2 :: IO ()
solveTest2 = readFile "testInput"
    >>= print
        . parseInput

solve2 :: IO ()
solve2 = readFile "input.txt"
    >>= print
        . parseInput

