module BinaryDiagnostic where

import Data.List (foldl', partition, transpose)

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

solveTest2 :: IO ()
solveTest2 = readFile "testInput"
    >>= print
        . parseInput

solve2 :: IO ()
solve2 = readFile "input.txt"
    >>= print
        . parseInput

