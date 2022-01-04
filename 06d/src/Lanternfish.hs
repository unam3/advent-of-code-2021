module Lanternfish where

import Data.List ((!!), foldl')
import GHC.List (iterate')
import Prelude hiding ((!!))

parseInput :: String -> [Int]
parseInput = read . ("[" ++) . (++ "]")

type CreateNewLanternfish = Bool

simulateDayCycle' :: Int -> (Int, CreateNewLanternfish)
simulateDayCycle' 0 = (6, True)
simulateDayCycle' number = (number - 1, False) 

-- theNumber — number that represents the number of days until it creates a new lanternfish
foldlF :: ([Int], Int) -> Int -> ([Int], Int)
foldlF (accList, numberOfFishToAdd) theNumber =
    let (nextTheNumber, doCreateNewLanternfish) = simulateDayCycle' theNumber
    in (
        nextTheNumber : accList,
        (if doCreateNewLanternfish
            then numberOfFishToAdd + 1
            else numberOfFishToAdd
        )
    )

simulateDayCycle :: [Int] -> [Int]
simulateDayCycle theNumbersList =
    let (newTheNumbersList, numberOfFishToAdd) = foldl' foldlF ([], 0) theNumbersList
    in (reverse newTheNumbersList) ++ (replicate numberOfFishToAdd 8)

simulateNDayCycles :: Int -> [Int] -> [Int]
simulateNDayCycles dayCyclesNumber theNumbersList = (!!) (iterate' simulateDayCycle theNumbersList) dayCyclesNumber

getNumberOfLanternsifhAfterNDays :: Int -> String -> Int
getNumberOfLanternsifhAfterNDays dayCyclesNumber theNumbersString =
    length  
        . simulateNDayCycles dayCyclesNumber
        $ parseInput theNumbersString


solveTest :: IO ()
solveTest = readFile "testInput"
    >>= print
        . getNumberOfLanternsifhAfterNDays 80

solve :: IO ()
solve = readFile "input.txt"
    >>= print
        . getNumberOfLanternsifhAfterNDays 80

solveTest2 :: IO ()
solveTest2 = readFile "testInput"
    >>= print
        . parseInput

solve2 :: IO ()
solve2 = readFile "input.txt"
    >>= print
        . parseInput

