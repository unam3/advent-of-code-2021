module Lanternfish where

import Data.List ((!!), foldl')
import Debug.Trace (trace)
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
    -- if we have 0 -> lookup 7: 7 + 0 or 0
    -- noop on 0
    let newMap =
            (if internalTimer == 0
            -- timersCount of 0 goes to 6 + timerscount of 7
            -- goes timersCount to 8
            then case lookup 7 oldLanternFishMap of
                Nothing ->
                    trace
                        (show ("nothing", timersCount, oldLanternFishMap, newLanternFishMap))
                        $ insert 8 timersCount
                            $ insert 6 timersCount newLanternFishMap
                Just sevenTimersCount ->
                    trace
                        (show (sevenTimersCount, '+', timersCount, oldLanternFishMap, newLanternFishMap))
                        $ insert 8 timersCount
                            $ insert 6 (sevenTimersCount + timersCount) newLanternFishMap
            else if internalTimer == 7
                then if member 0 oldLanternFishMap
                    then trace
                        "internalTimer == 7 — noop"
                        newLanternFishMap -- noop
                    else insert 6 timersCount newLanternFishMap
                -- timersCount goes to internalTimer - 1
                else trace
                        (show (internalTimer - 1, timersCount, oldLanternFishMap, newLanternFishMap))
                        $ insert (internalTimer - 1) timersCount newLanternFishMap)
    in (oldLanternFishMap, newMap)

simulateDayCycle :: LanternFishMap -> LanternFishMap
simulateDayCycle lanternFishMap = snd $ foldlWithKey' foldlWithKeyF (lanternFishMap, empty) lanternFishMap

simulateNDayCycles :: Int -> LanternFishMap -> LanternFishMap
simulateNDayCycles dayCyclesNumber lanternFishMap = (!!) (iterate' simulateDayCycle lanternFishMap) dayCyclesNumber

--getNumberOfLanternsifhAfterNDays :: Int -> String -> Int
--getNumberOfLanternsifhAfterNDays dayCyclesNumber theNumbersString =
--    length  
--        . simulateNDayCycles dayCyclesNumber
--        $ parseInput theNumbersString
--
--
--solveTest :: IO ()
--solveTest = readFile "testInput"
--    >>= print
--        . getNumberOfLanternsifhAfterNDays 80
--
--solve :: IO ()
--solve = readFile "input.txt"
--    >>= print
--        . getNumberOfLanternsifhAfterNDays 80
--
--solveTest2 :: IO ()
--solveTest2 = readFile "testInput"
--    >>= print
--        . getNumberOfLanternsifhAfterNDays 256
--
--solve2 :: IO ()
--solve2 = readFile "input.txt"
--    >>= print
--        . parseInput
--
