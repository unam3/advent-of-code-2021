module BinaryDiagnostic where

import Data.List (foldl', partition, transpose)
import Debug.Trace (trace)

parseInput :: String -> [String]
parseInput = transpose . lines

-- gammaRate calculation approaches

-- IntMap with culumn as key and (numberOfZeroes, numberOfOnes)
-- [(numberOfZeroes, numberOfOnes)]
-- transpose original input: each line of the list will correspond to original list column


getTupleListsLenghts :: (String, String) -> (Int, Int)
getTupleListsLenghts (zeroes, ones) = (length zeroes, length ones)

decideWhichBitMostCommon :: (Int, Int) -> Int
-- can't they be equal? we will suppose so.
decideWhichBitMostCommon tuple = if uncurry (<) tuple
    then 1
    else 0

getColumnGammaRate :: String -> Int
getColumnGammaRate =
    decideWhichBitMostCommon
        . getTupleListsLenghts
        -- "011110011100" -> ("00000","1111111")
        . partition (== '0')

-- https://stackoverflow.com/a/26961027/3484083
getDecimal :: [Int] -> Int
getDecimal = foldl' (\acc x -> acc * 2 + x) 0


infervseBit :: Int -> Int
infervseBit 0 = 1
infervseBit 1 = 0
infervseBit nonMatched = error $ "non matched int: " ++ show nonMatched

getDecimalEpsilonRate :: [Int] -> Int
getDecimalEpsilonRate = getDecimal . fmap infervseBit

powerConsumption :: [Int] -> Int
powerConsumption gammaRateBinary =
    let gammaRateDecimal = getDecimal gammaRateBinary
        epsilonRateDecimal = getDecimalEpsilonRate gammaRateBinary
    in gammaRateDecimal * epsilonRateDecimal

solveTest :: IO ()
solveTest = readFile "testInput"
    >>= print
        . powerConsumption
        . fmap getColumnGammaRate
        . parseInput

solve :: IO ()
solve = readFile "input.txt"
    >>= print
        . powerConsumption
        . fmap getColumnGammaRate
        . parseInput


determineMostCommonValue :: (Int, Int) -> Maybe Int
determineMostCommonValue (zeroesCount, onesCount)
    | zeroesCount == onesCount = Nothing
    | zeroesCount > onesCount = Just 0
    | otherwise = Just 1
    

o2BitCriteria :: String -> Char
o2BitCriteria column =
    -- "011110011100" -> ("00000","1111111")
    let partitions = partition (== '0') column
    in case determineMostCommonValue $ getTupleListsLenghts partitions of
        Just 0 -> '0'
        Just 1 -> '1'
        -- If 0 and 1 are equally common, keep values with a 1 in the position being considered.
        Nothing -> '1'
        nonMatched -> error $ "non matched" ++ show nonMatched

filterFunction :: Int -> Char -> String -> Bool
filterFunction position referenceBit string =
    let bitInAPosition = (!!) string position
    in bitInAPosition == referenceBit

oxygenGeneratorRating' :: Int -> [String] -> [String]
oxygenGeneratorRating' _ [onlyOneString] = [onlyOneString]
oxygenGeneratorRating' position stringList =
    let consideredColumnNumber = position
        referenceBit = o2BitCriteria . (!! consideredColumnNumber) $ transpose stringList
        result = filter (filterFunction position referenceBit) stringList
    --in trace
    --    (show (position, stringList, consideredColumnNumber, referenceBit))
    --    $ oxygenGeneratorRating' (position + 1) result
    in oxygenGeneratorRating' (position + 1) result
        

oxygenGeneratorRating :: String -> Int
oxygenGeneratorRating =
    getDecimal
        . fmap (
            (read :: String -> Int)
                . (:[])
            )
        . head
        . oxygenGeneratorRating' 0
        . lines

cO2ScrubberCriteria :: String -> Char
cO2ScrubberCriteria column =
    let partitions = partition (== '0') column
    in case determineMostCommonValue $ getTupleListsLenghts partitions of
        Just 0 -> '1'
        Just 1 -> '0'
        -- If 0 and 1 are equally common, keep values with a 1 in the position being considered.
        Nothing -> '0'
        nonMatched -> error $ "non matched" ++ show nonMatched

cO2ScrubberRating' :: Int -> [String] -> [String]
cO2ScrubberRating' _ [onlyOneString] = [onlyOneString]
cO2ScrubberRating' position stringList =
    let consideredColumnNumber = position
        referenceBit = cO2ScrubberCriteria . (!! consideredColumnNumber) $ transpose stringList
        result = filter (filterFunction position referenceBit) stringList
    --in trace
    --    (show (position, stringList, consideredColumnNumber, referenceBit))
    --    $ cO2ScrubberRating' (position + 1) result
    in cO2ScrubberRating' (position + 1) result
        

cO2ScrubberRating :: String -> Int
cO2ScrubberRating =
    getDecimal
        . fmap (
            (read :: String -> Int)
                . (:[])
            )
        . head
        . cO2ScrubberRating' 0
        . lines

getLifeSupportRating :: String -> Int
-- can we use point-free here?
--getLifeSupportRating = fmap (\f -> f) [oxygenGeneratorRating, cO2ScrubberRating]
getLifeSupportRating inputString =
    oxygenGeneratorRating inputString * cO2ScrubberRating inputString

solveTest2 :: IO ()
solveTest2 = readFile "testInput"
    >>= print
        . getLifeSupportRating

solve2 :: IO ()
solve2 = readFile "input.txt"
    >>= print
        . getLifeSupportRating

