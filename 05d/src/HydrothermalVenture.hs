module HydrothermalVenture where

import Data.List (foldl')
import qualified Data.Map.Strict as Map

type XYPoint = (Int, Int)
type LineDefinition = (XYPoint, XYPoint)

getOnlyCoordsStrings :: [String] -> [String]
getOnlyCoordsStrings [s, _, s1] = [s, s1]
getOnlyCoordsStrings wrongInput = error $ show wrongInput

parseXYPoints :: [String] -> LineDefinition
parseXYPoints =
    (\ [(x, y'), (x1, y1')] -> ((read x, read $ tail y'), (read x1, read $ tail y1')))
        . fmap (break (== ','))

parseInput :: String -> [LineDefinition]
parseInput = fmap (parseXYPoints . getOnlyCoordsStrings . words) . lines

isHVDefinition :: LineDefinition -> Bool
isHVDefinition ((x, y), (x1, y1)) = x == x1 || y == y1

getHVLinePoints :: LineDefinition -> [XYPoint]
getHVLinePoints ((x, y), (x1, y1)) =
    if x == x1
    then fmap
        (\ y' -> (x, y'))
            (if y > y1
            then [y1..y]
            else [y..y1])
    else fmap
        (\ x' -> (x', y))
            (if x > x1
            then [x1..x]
            else [x..x1])

type VentPointsMap = Map.Map XYPoint Int

addOrIterateNumberOfLinesPointIs :: VentPointsMap -> XYPoint -> VentPointsMap
addOrIterateNumberOfLinesPointIs ventPointsAcc xyPoint = Map.insertWith (+) xyPoint 1 ventPointsAcc

markLinePoints :: VentPointsMap -> LineDefinition -> VentPointsMap
markLinePoints ventPointsMap lineDefinition =
    let linePoints = getHVLinePoints lineDefinition
    in foldl' addOrIterateNumberOfLinesPointIs ventPointsMap linePoints

makeVentPointsMap :: [LineDefinition] -> VentPointsMap
makeVentPointsMap = foldl' markLinePoints Map.empty

getPointsWithOverlappedLines :: VentPointsMap -> VentPointsMap
getPointsWithOverlappedLines = Map.filter (> 1)

solveTest :: IO ()
solveTest = readFile "testInput"
    >>= print
        . length
        . getPointsWithOverlappedLines
        . makeVentPointsMap
        . filter isHVDefinition
        . parseInput

solve :: IO ()
solve = readFile "input.txt"
    >>= print
        . length
        . getPointsWithOverlappedLines
        . makeVentPointsMap
        . filter isHVDefinition
        . parseInput

solveTest2 :: IO ()
solveTest2 = readFile "testInput"
    >>= print
        . parseInput

solve2 :: IO ()
solve2 = readFile "input.txt"
    >>= print
        . parseInput

