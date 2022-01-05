module Lanternfish where

import Data.List ((!!), foldl')
import Data.Map.Strict (Map, empty, foldlWithKey', insert, insertWith, lookup, member)
import GHC.List (iterate')
import Prelude hiding ((!!), lookup)


type InternalTimer = Int
type TimersCount = Int
type LanternFishMap = Map InternalTimer TimersCount

parseString :: String -> [Int]
parseString = read . ("[" ++) . (++ "]")


foldlF :: LanternFishMap -> InternalTimer -> LanternFishMap
foldlF lanternFishMap internalTimer = insertWith (+) internalTimer 1 lanternFishMap

makeLanternFishMap :: [Int] -> LanternFishMap
makeLanternFishMap = foldl' foldlF empty

parseInput :: String -> LanternFishMap
parseInput = makeLanternFishMap . parseString


{-
0 —— 13  ——> 0 —— 5
1 —— 5   ——> 1 —— ~ 
..       ——> ..    
5 —— 11  ——> 5 —— 32
6 —— 32  ——> 6 —— 29 + 13 (0)
7 —— 29  ——> 7 —— 17
8 —— 17  ——> 8 —— 13
-}
foldlWithKeyF :: (LanternFishMap, LanternFishMap) -> InternalTimer -> TimersCount -> (LanternFishMap, LanternFishMap)
foldlWithKeyF (oldLanternFishMap, newLanternFishMap) internalTimer timersCount =
    let newMap |
            internalTimer == 0
            = case lookup 7 oldLanternFishMap of
                Nothing ->
                    insert 8 timersCount
                        $ insert 6 timersCount newLanternFishMap
                Just sevenTimersCount ->
                    insert 8 timersCount
                        $ insert 6 (sevenTimersCount + timersCount) newLanternFishMap
            | internalTimer == 7
            = if member 0 oldLanternFishMap
                    then newLanternFishMap
                    else insert 6 timersCount newLanternFishMap
            | otherwise
            = insert (internalTimer - 1) timersCount newLanternFishMap
    in (oldLanternFishMap, newMap)

simulateDayCycle :: LanternFishMap -> LanternFishMap
simulateDayCycle lanternFishMap = snd $ foldlWithKey' foldlWithKeyF (lanternFishMap, empty) lanternFishMap

simulateNDayCycles :: Int -> LanternFishMap -> LanternFishMap
simulateNDayCycles dayCyclesNumber lanternFishMap = (!!) (iterate' simulateDayCycle lanternFishMap) dayCyclesNumber

getNumberOfLanternsifhAfterNDays :: Int -> String -> Int
getNumberOfLanternsifhAfterNDays dayCyclesNumber theNumbersString =
    foldl' (+) 0
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
        . getNumberOfLanternsifhAfterNDays 256

solve2 :: IO ()
solve2 = readFile "input.txt"
    >>= print
        . getNumberOfLanternsifhAfterNDays 256
