module GiantSquid where

import Data.List (elemIndex, foldl', nub)
--import Data.Map.Strict (member)
import Data.Map.Strict hiding (foldl', null, splitAt)

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
        Just (left, right') -> left : splitInput (removeTwoNewlines right')
        _ -> [input]


type NumbersToDraw = [Int]

parseNumbersToDraw :: String -> NumbersToDraw
parseNumbersToDraw =
    read
        . ("[" ++)
        . (++ "]")


type BoardIntList = [[Int]]

-- where [String] is boardList
parseBoards :: [String] -> [BoardIntList]
parseBoards = fmap (fmap (fmap (read :: String -> Int) . words) . lines)

type XYTuple = (Int, Int)
-- to mark and check drawn numbers on the map
type MarkedCoordinatesMap = Map XYTuple Bool

assignMarkedCoordinatesMap :: BoardIntList -> (BoardIntList, MarkedCoordinatesMap)
assignMarkedCoordinatesMap boardIntList = (boardIntList, empty)

type BoardWithState = (BoardIntList, MarkedCoordinatesMap)

parseInput :: [String] -> (NumbersToDraw, [BoardWithState])
parseInput splittedInput =
    let (numbersToDrawStringList, boardStringList) = splitAt 1 splittedInput
        numbersToDraw = parseNumbersToDraw $ head numbersToDrawStringList
        boardsList = parseBoards boardStringList
    in (numbersToDraw, fmap assignMarkedCoordinatesMap boardsList)


findNumberCoordinatesInRow :: Int -> (Int, [Int]) -> Maybe XYTuple
findNumberCoordinatesInRow numberToFind (rowNumber, row) =
        fmap (\ colIndex -> (rowNumber, colIndex))
            $ elemIndex numberToFind row


findNumberCoordinatesInRows :: Int -> [(Int, [Int])] -> Maybe XYTuple
findNumberCoordinatesInRows numberToFind (row : rows) = 
    let result = findNumberCoordinatesInRow numberToFind row
    in case result of
        Nothing -> findNumberCoordinatesInRows numberToFind rows
        jusCoordinates -> jusCoordinates
findNumberCoordinatesInRows _ [] = Nothing

-- possible implementations:
--  - add to the number in position its coordinates (with zip [0..] to go through lines of boardIntList) and use findIndices
--  - recourse and increment the counter by hands
-- the simplest one is the first variant


findNumberCoordinates :: Int -> BoardIntList -> Maybe XYTuple
findNumberCoordinates numberToMark boardIntList =
    let numberedRows =  zip [0..] boardIntList
        coordinateToMark = findNumberCoordinatesInRows numberToMark numberedRows
    in coordinateToMark

markCoordinates :: MarkedCoordinatesMap -> XYTuple -> MarkedCoordinatesMap
markCoordinates markedCoordinatesMap xy = insert xy True markedCoordinatesMap

checkIfBoardWin :: MarkedCoordinatesMap -> XYTuple -> Bool
checkIfBoardWin markedCoordinatesMap (rowNumber, columnNumber) =
    let row = fmap (\coordinate -> (rowNumber, coordinate)) [0..4]
        column = fmap (\coordinate -> (coordinate, columnNumber)) [0..4]
        rowIsFullyMarked = all (`member` markedCoordinatesMap) row
        columnIsFullyMarked = all (`member` markedCoordinatesMap) column
    in rowIsFullyMarked || columnIsFullyMarked


drawNumberUntilPossibleWin :: NumbersToDraw -> BoardWithState -> Maybe Int
drawNumberUntilPossibleWin (drawnNumber : restNumbersToDraw) boardWithState@(boardIntList, markedCoordinatesMap) =
    case findNumberCoordinates drawnNumber boardIntList of
        Nothing -> drawNumberUntilPossibleWin restNumbersToDraw boardWithState
        Just drawnNumberCoordinates ->
            let updatedMarkedCoordinatesMap = markCoordinates markedCoordinatesMap drawnNumberCoordinates
                isBoardWin = checkIfBoardWin updatedMarkedCoordinatesMap drawnNumberCoordinates
            in if isBoardWin
                then Just drawnNumber
                else drawNumberUntilPossibleWin restNumbersToDraw (boardIntList, updatedMarkedCoordinatesMap)
drawNumberUntilPossibleWin [] _ = Nothing


isAnyRowOfBorderHaveSameNumberSeveralTimes :: BoardIntList -> Bool
isAnyRowOfBorderHaveSameNumberSeveralTimes = any ((/= 5) . length . nub)

isBorderHaveSameNumberSeveralTimes :: BoardIntList -> Bool
isBorderHaveSameNumberSeveralTimes = (/= 25) . length . nub . concat

solveTest :: IO ()
solveTest = readFile "testInput"
    >>= print
        -- . any isBorderHaveSameNumberSeveralTimes
        -- -- [BoardIntList]
        -- . fmap fst
        -- -- [BoardWithState]
        -- . snd
        . parseInput
        . splitInput

solve :: IO ()
solve = readFile "input.txt"
    >>= print
        -- . any isBorderHaveSameNumberSeveralTimes
        -- -- [BoardIntList]
        -- . fmap fst
        -- -- [BoardWithState]
        -- . snd
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

