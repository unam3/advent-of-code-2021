module DumboOctopus where

import Data.Bifunctor (second)
import Data.List ((\\), foldl')
import Data.Map.Strict (Map, adjust, filter, fromList, keys)
import Prelude hiding (filter)

charToInt :: Char -> Int
charToInt = read . (: [])

type EnergyLevels = Map (Int, Int) Int

parseInput :: String -> EnergyLevels
parseInput = 
    fromList
        -- rewrite simpler?
        . concatMap (
            (\(y, xAndCharList) -> fmap (\(x, char) -> ((x, y), charToInt char)) xAndCharList)
                . second (zip [0..]))
        . zip [0..]
        . lines

type TotalFlashes = Int
type State = (EnergyLevels, TotalFlashes)

increaseAdjacentLevels :: EnergyLevels -> (Int, Int) -> EnergyLevels
increaseAdjacentLevels energyLevels (x, y) =
    let adjacentPositionFunctions = [
                (id,         (+1)),       -- N
                ((+1),       (+1)),       -- NE
                ((+1),       id),         -- E
                ((+1),       subtract 1), -- SE
                (id,         subtract 1), -- S
                (subtract 1, subtract 1), -- SW
                (subtract 1, id),         -- W
                (subtract 1, (+1))        -- NW
            ]
    in foldl'
        (\accMap (xAdjacentF, yAdjacentF) -> adjust (+1) (xAdjacentF x, yAdjacentF y) accMap)
        energyLevels
        adjacentPositionFunctions

filterIncreaseLoop :: EnergyLevels -> ([(Int, Int)], [(Int, Int)]) -> (EnergyLevels, ([(Int, Int)], [(Int, Int)]))
filterIncreaseLoop energyLevels coords@(_, []) = (energyLevels, coords)
filterIncreaseLoop energyLevels (flashedWithoutLastCoords, lastFlashedCoords) =
    -- icrease EL of adjacent to flashed ones octupuses 
    let withIncreasedAdjacentLevels = foldl' increaseAdjacentLevels energyLevels lastFlashedCoords
        -- check for ones that had prev LE below 9 and now it's greater 9
        withNewFlashes = filter (> 9) withIncreasedAdjacentLevels
        -- takes into account previously flashed octopuses
        onlyAdjacentFlashesCoords = (\\ lastFlashedCoords)
            $ (\\) (keys withNewFlashes) flashedWithoutLastCoords
    in filterIncreaseLoop
        withIncreasedAdjacentLevels
        (flashedWithoutLastCoords ++ lastFlashedCoords , onlyAdjacentFlashesCoords)

simulateStep :: State -> State
simulateStep (energyLevels, totalFlashes) =
    -- the energy level of each octopus increases by 1.
    let increasedEnergyLevels = fmap (+1) energyLevels
        flashes = filter (> 9) increasedEnergyLevels
        (newEL, (octopusesThatFlashesThisStep, _)) = filterIncreaseLoop increasedEnergyLevels ([], keys flashes)
        newTotalFlashes = totalFlashes + length octopusesThatFlashesThisStep
    in (
        -- zero ctopuses with energy level > 9
        fmap
            (\ energyLevel -> 
                if energyLevel > 9
                then 0
                else energyLevel
            )
            newEL,
        newTotalFlashes
    )

simulateNSteps :: State -> Int -> State
simulateNSteps state 0 = state
simulateNSteps state n = simulateNSteps (simulateStep state) (n - 1)

solve :: String -> State
solve inputString = simulateNSteps (parseInput inputString, 0) 100

--solveTest2 :: IO ()
--solveTest2 = readFile "testInput"
--    >>= print
--        . parseInput
--
--solve2 :: IO ()
--solve2 = readFile "input.txt"
--    >>= print
--        . parseInput
--
