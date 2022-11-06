module DumboOctopus where

--import Data.Maybe (catMaybes)
--import Text.Read (readMaybe)
import Data.List ((\\), foldl')
import Data.Map.Strict (Map, adjust, filter, fromList, keys, size)
import Prelude hiding (filter)

charToInt :: Char -> Int
charToInt = read . (: [])

type EnergyLevels = Map (Int, Int) Int

parseInput :: String -> EnergyLevels
parseInput = 
    fromList
        -- rewrite simpler
        . concat
        . fmap (\(y, xAndCharList) -> fmap (\(x, char) -> ((x, y), charToInt char)) xAndCharList)
        . fmap
            (\ (y, numberString) ->
                (y, zip [0..] numberString)
            )
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

filterIncreaseLoop :: EnergyLevels -> [(Int, Int)] -> EnergyLevels
filterIncreaseLoop energyLevels [] = energyLevels
filterIncreaseLoop energyLevels flashesCoords =
    -- icrease EL of adjacent to flashed ones octupuses 
    let withIncreasedAdjacentLevels = foldl' increaseAdjacentLevels energyLevels flashesCoords
        -- check for ones that had prev LE below 9 and now it's greater 9
        withNewFlashes = filter (\ v -> v > 9) withIncreasedAdjacentLevels
        onlyAdjacentFlashesCoords = (\\) (keys withNewFlashes) flashesCoords
    in filterIncreaseLoop withNewFlashes onlyAdjacentFlashesCoords

simulateStep :: State -> State
simulateStep (energyLevels, totalFlashes) =
    -- the energy level of each octopus increases by 1.
    let increasedEnergyLevels = fmap (+1) energyLevels
        flashes = filter (\ v -> v > 9) increasedEnergyLevels
        newEL = filterIncreaseLoop increasedEnergyLevels (keys flashes)
        -- count number of octopuses with energy level > 9 and add to totalFlashes
        octopusesThatFlashesThisStep = filter (\ v -> v > 9) newEL
        newTotalFlashes = totalFlashes + (size $ octopusesThatFlashesThisStep)
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


--solveTest :: IO ()
--solveTest = readFile "testInput"
--    >>= print
--        . parseInput
--
--solve :: IO ()
--solve = readFile "input.txt"
--    >>= print
--        . parseInput
--
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
