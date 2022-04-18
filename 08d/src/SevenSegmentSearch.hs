module SevenSegmentSearch where


import Data.List (elemIndex, foldl', sort, union, (\\))

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

1) identify T (top) segment representation:
    if we have 1 ab and 7 abc then top is "c"

2) get 9 from (1,4,7):
    if we have 1 ab and 4 abcd then top left and middle are either "c" or "d"
        and this is useful for deducing 5, 6, 8 and 9

3) derive bottom from 9

4) deduce 0

5) from 0 derive borrom left BL and middle M

6) make 3 from 9


if we have 1, 4 and 7 we need only one side for 9

n    number of lines
1 - 2 exclusively
7 - 3 exclusively
4 - 4 exclusively
2 - 5
3 - 5
5 - 5
6 - 6
0 - 6
9 - 6
8 - 7 exclusively

1 (TR, BR)          is in 0, 1, 3, 4, 7, 8, 9
4 (TR, BR, M, TL)   is in 4, 8, 9
7 (T, TR, BR)       is in 0, 3, 7, 8, 9

If we have (BL, B), (TL, M) how we can know for sure which is which?


Let's examine 6: it hadn't only TR segment and we can find it by excluding parts of 1 from 8 and analyze all ten unique representations: which one hasn't segment of 1 will be the 6. Also by doing that we can identify TR and BR segments

5 as is has no TR segment, so we can identify B and then BL.

-}


deriveTopFrom1And7 :: String -> String -> String
deriveTopFrom1And7 one seven = 
    let c = seven \\ one
    in "'" ++ show c ++ "' is the top segment"


getFixedLengthDefinition :: Int -> [String] -> String
getFixedLengthDefinition definitionLength = head . filter ((== definitionLength) . length)

get1 :: [String] -> String
get1 = getFixedLengthDefinition 2

get4 :: [String] -> String
get4 = getFixedLengthDefinition 4

get7 :: [String] -> String
get7 = getFixedLengthDefinition 3

get8 :: [String] -> String
get8 = getFixedLengthDefinition 7

derive6From8And1 :: String -> String -> [String] -> String
derive6From8And1 eight [o, o1] uniquePatterns =
    let eitherSix = eight \\ (show o)
        orSix = eight \\ (show o1)
        msg = "6 definition is "
    in case elemIndex eitherSix uniquePatterns of
        Just _ -> msg ++ eitherSix
        _ -> msg ++ orSix

derive6From8And1 eight notValidInput _ = error "input of " ++ eight ++ ", " ++ notValidInput ++ " is not valid"


deriveBLAndBFrom147 :: String -> String -> String -> String
deriveBLAndBFrom147 one four seven = 
    let eitherLeftBottomOrBottom = (\\) "abcdefg" $ sort $ union seven $ union four one
    in "'" ++ eitherLeftBottomOrBottom ++ "' are the either left bottom or bottom segments of the seven-digit display"

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

