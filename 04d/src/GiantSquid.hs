module GiantSquid where

import Data.List (elemIndex, foldl', nub)
--import Data.Map.Strict (member)
import Data.Map.Strict hiding (null, foldl', splitAt)
import Prelude hiding (filter, lookup)

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

parseInput :: [String] -> (NumbersToDraw, BoardWithStateMap)
parseInput splittedInput =
    let (numbersToDrawStringList, boardStringList) = splitAt 1 splittedInput
        numbersToDraw = parseNumbersToDraw $ head numbersToDrawStringList
        boardsList = parseBoards boardStringList
    in (numbersToDraw, makeBoardWithStateMap $ fmap assignMarkedCoordinatesMap boardsList)


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


type BoardWithStateMap = Map Int BoardWithState

makeBoardWithStateMap :: [BoardWithState] -> BoardWithStateMap
makeBoardWithStateMap = fromList . zip [0..]

drawNumberUntilFirstPossibleWin' :: NumbersToDraw -> BoardWithStateMap -> Int -> Maybe (BoardWithState, Int, BoardWithStateMap)
drawNumberUntilFirstPossibleWin' numbersToDraw@(drawnNumber : restNumbersToDraw) boardWithStateMap boardWithStateIndex =
    case lookup boardWithStateIndex boardWithStateMap of
        Just (boardIntList, markedCoordinatesMap) ->
            case findNumberCoordinates drawnNumber boardIntList of
                Nothing -> drawNumberUntilFirstPossibleWin' numbersToDraw boardWithStateMap (boardWithStateIndex + 1)
                Just drawnNumberCoordinates ->
                    let updatedMarkedCoordinatesMap = markCoordinates markedCoordinatesMap drawnNumberCoordinates
                        isBoardWin = checkIfBoardWin updatedMarkedCoordinatesMap drawnNumberCoordinates
                        updatedBoardWithStateMap =
                            insert
                                boardWithStateIndex
                                (boardIntList, updatedMarkedCoordinatesMap)
                                boardWithStateMap
                    in if isBoardWin
                        then Just ((boardIntList, updatedMarkedCoordinatesMap), drawnNumber, updatedBoardWithStateMap)
                        else drawNumberUntilFirstPossibleWin'
                                numbersToDraw
                                updatedBoardWithStateMap
                                (boardWithStateIndex + 1)
        Nothing -> drawNumberUntilFirstPossibleWin' restNumbersToDraw boardWithStateMap 0
drawNumberUntilFirstPossibleWin' [] _ _ = Nothing

drawNumberUntilFirstPossibleWin :: (NumbersToDraw, BoardWithStateMap) -> Maybe (BoardWithState, Int, BoardWithStateMap)
drawNumberUntilFirstPossibleWin (numbersToDraw, boardWithStateMap) =
    drawNumberUntilFirstPossibleWin' numbersToDraw boardWithStateMap 0

addNumberIfUnmarked :: MarkedCoordinatesMap -> Int -> Int -> (Int, Int) -> Int
addNumberIfUnmarked markedCoordinatesMap y acc (x, number) =
    case lookup (y, x) markedCoordinatesMap of
        Just _ -> acc
        Nothing -> number + acc

sumRow :: MarkedCoordinatesMap -> Int -> (Int, [Int]) -> Int
sumRow markedCoordinatesMap acc (y, row) = 
    let rowUnmarkedNumbersSum = foldl' (addNumberIfUnmarked markedCoordinatesMap y) 0
            -- [(x, v),… (xn, vn)]
            $ zip [0..] row
    in rowUnmarkedNumbersSum + acc


sumUnmarkedNumbers :: BoardWithState -> Int
sumUnmarkedNumbers (boardIntList, markedCoordinatesMap) =
    foldl' (sumRow markedCoordinatesMap) 0
        -- [(y, row),… (yn, rown)]
        $ zip [0..] boardIntList

calculateWinningScore :: BoardWithState -> Int -> Int
calculateWinningScore boardWithState drawnNumber = sumUnmarkedNumbers boardWithState * drawnNumber


solveTest :: IO ()
solveTest = readFile "testInput"
    >>= print
        . (\ maybeBoardWithStateAndDrawnNumber ->
            case maybeBoardWithStateAndDrawnNumber of
                Just (boardWithState, drawnNumber, _) ->
                    calculateWinningScore boardWithState drawnNumber
                Nothing -> error "no winning board"
        )
        . drawNumberUntilFirstPossibleWin
        . parseInput
        . splitInput

solve :: IO ()
solve = readFile "input.txt"
    >>= print
        . (\ maybeBoardWithStateAndDrawnNumber ->
            case maybeBoardWithStateAndDrawnNumber of
                Just (boardWithState, drawnNumber, _) ->
                    calculateWinningScore boardWithState drawnNumber
                Nothing -> error "no winning board"
        )
        . drawNumberUntilFirstPossibleWin
        . parseInput
        . splitInput


getSublistAfterElement :: Int -> [Int] -> [Int]
getSublistAfterElement drawnNumber numbersToDraw = tail . snd $ break (== drawnNumber) numbersToDraw

-- type BoardWithState = (BoardIntList, MarkedCoordinatesMap)
-- type BoardWithStateMap = Map Int BoardWithState
removeBoard :: BoardWithStateMap -> BoardIntList -> BoardWithStateMap
removeBoard boardWithStateMap boardIntList = filter ((/= boardIntList) . fst) boardWithStateMap

drawNumberUntilLastPossibleWin' ::
    (NumbersToDraw, BoardWithStateMap)
    -> [Maybe (BoardWithState, Int, BoardWithStateMap)]
    -> [Maybe (BoardWithState, Int, BoardWithStateMap)]
drawNumberUntilLastPossibleWin' (numbersToDraw, boardWithStateMap) lastPossibleWinResultsList =
    let winResults = drawNumberUntilFirstPossibleWin (numbersToDraw, boardWithStateMap)
    in case winResults of
        Just (winnedBoardWithState, winnedDrawnNumber, winnedBoardWithStateMap) ->
            let updatedNumbersToDraw = getSublistAfterElement winnedDrawnNumber numbersToDraw
                updatedBoardWithStateMap = removeBoard winnedBoardWithStateMap $ fst winnedBoardWithState
                updatedWinResults = Just (winnedBoardWithState, winnedDrawnNumber, updatedBoardWithStateMap)
            in drawNumberUntilLastPossibleWin'
                (updatedNumbersToDraw, updatedBoardWithStateMap)
                (updatedWinResults : lastPossibleWinResultsList)
        Nothing -> lastPossibleWinResultsList

drawNumberUntilLastPossibleWin :: (NumbersToDraw, BoardWithStateMap) -> [Maybe (BoardWithState, Int, BoardWithStateMap)]
drawNumberUntilLastPossibleWin (numbersToDraw, boardWithStateMap) =
    drawNumberUntilLastPossibleWin' (numbersToDraw, boardWithStateMap) []


isAnyRowOfBorderHaveSameNumberSeveralTimes :: BoardIntList -> Bool
isAnyRowOfBorderHaveSameNumberSeveralTimes = any ((/= 5) . length . nub)

isBorderHaveSameNumberSeveralTimes :: BoardIntList -> Bool
isBorderHaveSameNumberSeveralTimes = (/= 25) . length . nub . concat

getBoardWithStateMap :: Maybe (BoardWithState, Int, BoardWithStateMap) -> BoardWithStateMap
getBoardWithStateMap (Just (_, _, boardWithStateMap)) = boardWithStateMap
getBoardWithStateMap _ = undefined

solveTest2 :: IO ()
solveTest2 = readFile "testInput"
    >>= print
        -- . (\ maybeBoardWithStateAndDrawnNumber ->
        --     case maybeBoardWithStateAndDrawnNumber of
        --         Just (boardWithState, drawnNumber) ->
        --             calculateWinningScore boardWithState drawnNumber
        --         Nothing -> error "no winning board"
        -- )

        . (\ (Just (winnedBoardWithState, winnedDrawnNumber, winnedBoardWithStateMap)) ->
            let updatedNumbersToDraw =
                    getSublistAfterElement
                        winnedDrawnNumber
                        ([7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1])
                updatedBoardWithStateMap = removeBoard winnedBoardWithStateMap $ fst winnedBoardWithState
            in drawNumberUntilFirstPossibleWin (updatedNumbersToDraw, updatedBoardWithStateMap)

        )
        
        . (\ (Just (winnedBoardWithState, winnedDrawnNumber, winnedBoardWithStateMap)) ->
            let updatedNumbersToDraw =
                    getSublistAfterElement
                        winnedDrawnNumber
                        ([7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1])
                updatedBoardWithStateMap = removeBoard winnedBoardWithStateMap $ fst winnedBoardWithState
            in drawNumberUntilFirstPossibleWin (updatedNumbersToDraw, updatedBoardWithStateMap)

        )
        
        -- . getBoardWithStateMap
        . drawNumberUntilFirstPossibleWin

        -- . getBoardWithStateMap
        -- . head
        -- . drawNumberUntilLastPossibleWin

        . parseInput
        . splitInput

solve2 :: IO ()
solve2 = readFile "input.txt"
    >>= print
        . parseInput
        . splitInput

