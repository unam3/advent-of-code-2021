module GiantSquid where

import Data.Map.Strict hiding (splitAt)

getEmptyStringPosition' :: String -> Int -> Maybe Int
getEmptyStringPosition' ('\n':'\n':_) step = Just step
getEmptyStringPosition' (_:c1:rest) step = getEmptyStringPosition' (c1 : rest) (step + 1)
getEmptyStringPosition' "\n" _ = Nothing
getEmptyStringPosition' input step = error $ "error on step: " ++ show (step, input)

getEmptyStringPosition :: String -> Maybe Int
getEmptyStringPosition string = getEmptyStringPosition' string 0

splitOnEmptyString :: String -> Maybe (String, String)
splitOnEmptyString string =
    let emptyStringPosition = getEmptyStringPosition string
    in case emptyStringPosition of
        Just position -> Just $ splitAt position string
        Nothing -> Nothing

removeTwoNewlines :: String -> String
removeTwoNewlines ('\n':'\n':rest) = rest
removeTwoNewlines nonMatchedInput =
    error ("nonMatchedInput: " ++ show nonMatchedInput)

splitInput :: String -> [String]
splitInput input =
    let splitResults = splitOnEmptyString input
    in case splitResults of
        Just (left, right') -> [left] ++ (splitInput $ removeTwoNewlines right')
        _ -> [input]


type NumbersToDraw = [Int]

parseNumbersToDraw :: String -> NumbersToDraw
parseNumbersToDraw =
    read
        . ("[" ++)
        . (++ "]")


type BoardIntList = [[Int]]

parseBoards :: [String] -> [BoardIntList]
parseBoards boardList = fmap ((fmap $ fmap (read :: String -> Int)) . fmap words . lines) boardList

type Coordinates = (Int, Int)
-- to mark and check drawn numbers on the map
type BoardMap = Map (Int, Int) Bool

assignBoardMap :: BoardIntList -> (BoardIntList, BoardMap)
assignBoardMap boardIntList = (boardIntList, empty)

parseInput :: [String] -> (NumbersToDraw, [(BoardIntList, BoardMap)])
parseInput splittedInput =
    let (numbersToDrawStringList, boardList) = splitAt 1 splittedInput
        numbersToDraw = parseNumbersToDraw $ head numbersToDrawStringList
        boards = parseBoards boardList
    in (numbersToDraw, fmap assignBoardMap boards)

-- [[3,15,0,2,22],[9,18,13,17,5],[19,8,7,25,23],[20,11,10,24,4],[14,21,16,12,6]]
-- fmap (zip [0..]) boardIntList
-- [[(0,3),(1,15),(2,0),(3,2),(4,22)],[(0,9),(1,18),(2,13),(3,17),(4,5)],[(0,19),(1,8),(2,7),(3,25),(4,23)],[(0,20),(1,11),(2,10),(3,24),(4,4)],[(0,14),(1,21),(2,16),(3,12),(4,6)]]

-- zip [0..] $ fmap (zip [0..]) boardIntList
-- [(0,[(0,3),(1,15),(2,0),(3,2),(4,22)]),(1,[(0,9),(1,18),(2,13),(3,17),(4,5)]),(2,[(0,19),(1,8),(2,7),(3,25),(4,23)]),(3,[(0,20),(1,11),(2,10),(3,24),(4,4)]),(4,[(0,14),(1,21),(2,16),(3,12),(4,6)])]

-- if zip on the fly will be slow — we can do all zipping in the parseInput once
markNumber :: Int -> (BoardIntList, BoardMap) -> (BoardIntList, BoardMap)
markNumber numberToMark (boardIntList, boardMap) =
    foldl (\acc (lineNumber, tupleList) -> let ei = elemIndices 15 in if null ei then acc else ei ++ acc) [] $ zip [0..] $ fmap (zip [0..]) boardIntList


solveTest :: IO ()
solveTest = readFile "testInput"
    >>= print
        . parseInput
        . splitInput

solve :: IO ()
solve = readFile "input.txt"
    >>= print
        . parseInput
        . splitInput

solveTest2 :: IO ()
solveTest2 = readFile "testInput"
    >>= print
        . parseInput
        . splitInput

solve2 :: IO ()
solve2 = readFile "input.txt"
    >>= print
        . parseInput
        . splitInput

