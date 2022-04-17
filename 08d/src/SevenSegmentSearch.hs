module SevenSegmentSearch where


import Data.List (foldl', sort, union, (\\))

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


normalize :: String -> String
normalize = unwords . fmap sort . words

p2ParseInput :: String -> [String]
p2ParseInput = fmap normalize . lines

{-

Deduce four digits of the output value and decode them.

1) derive top:
    if we have 1 ab and 7 abc then top is "c"

2) deduce 9 from (1,4,7):
    if we have 1 ab and 4 abcd then top left and middle are either "c" or "d"
        and this is useful for deducing 5, 6, 8 and 9

3) derive bottom from 9

4) deduce 0

5) from 0 derive borrom left BL and middle M

6) make 3 from 9


if we have 1, 4 and 7 we need only one side for 9

n    number of lines
0 -  6
1 - u2
2 -  5
3 -  5
4 - u4
5 -  5
6 -  6
7 - u3
8 - u7
9 - 6

-}



deriveTopFrom1And7 :: (Maybe String, Maybe String) -> String
deriveTopFrom1And7 (Just one, Just seven) = 
    let c = seven \\ one
    in "'" ++ show c ++ "' is the top line of the seven-digit display"
deriveTopFrom1And7 (maybeOne, maybeSeven) = "can't derive top from 1 and 7: " ++ show (maybeOne, maybeSeven)

deriveBLAndBFrom147 :: (Maybe String, Maybe String, Maybe String) -> String
deriveBLAndBFrom147 (Just one, Just four, Just seven) = 
    let eitherLeftBottomOrBottom = (\\) "abcdefg" $ sort $ union seven $ union four one
    in "'" ++ eitherLeftBottomOrBottom ++ "' are the either left bottom or bottom lines of the seven-digit display"

deriveBLAndBFrom147 (maybeOne, maybeFour, maybeSeven) =
    "can't derive top from 1 and 7: " ++ show (maybeOne, maybeFour, maybeSeven)

--deriveRepresentationFrom1478 :: (Maybe String, Maybe String, Maybe String, Maybe String) -> String
--deriveRepresentationFrom1478 (maybeOne, maybeFour, maybeSeven, maybeEight) =
--    case (one, four) of
--        (Just one, Just four) ->
--        _ -> "nothing insightful"

--addUpAllOutputValues = undefined


solveTest2 :: IO ()
solveTest2 = readFile "testInput"
    >>= print
        . p2ParseInput

solve2 :: IO ()
solve2 = readFile "input.txt"
    >>= print
        . parseInput

