module TheTreacheryofWhales where


import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl')

{-
Main questions to answer:

1) Determine the horizontal position that the crabs can align to using the least fuel possible.
    • brute force from least to biggest horizontal positions?
    - any analytical solutions?
        656 number of positions but there can be intermediate numbers between them
2) How much fuel must they spend to align to that position. -implemented
    

How represent input numbers? -CrabsMap
How represent fuel-position values? -tuple of NumberOfCrabsMapInThisPosition, Maybe Fuel

-}

type NumberOfCrabsMapInThisPosition = Int -- will be our multiplier
type Fuel = Int

-- key -- horizontal position
type CrabsMap = IntMap (NumberOfCrabsMapInThisPosition, Maybe Fuel)

-- How do we get list iof ints from the imput?
--  + concat square brackets and use readEither
--  - replace commas with whitespaces and use words, then read


type HorizontalPosition = Int

foldF :: CrabsMap -> HorizontalPosition -> CrabsMap
foldF crabsMap horizontalPosition =
    IntMap.insertWith
        (
            \ _ (oldNumberOfCrabsInThisPosition, oldMaybeFuel) -> (oldNumberOfCrabsInThisPosition + 1, oldMaybeFuel)
        )
        horizontalPosition
        (1, Nothing)
        crabsMap


parseInput :: String -> CrabsMap
parseInput inputString =
    let crabPositionsList = (read $ concat ["[", inputString, "]"] :: [HorizontalPosition])
    in foldl' foldF IntMap.empty crabPositionsList


-- how to determine how many fuel we need to spend to get to alignment position?
-- 1) ap == horizontalPosition -> 0
-- 2) ap > horizontalPosition -> ap 20, hp 10 → 10
-- 3) ap < horizontalPosition -> ap 10, hp 20 → 10 

alignFoldF :: HorizontalPosition -> Int -> Int -> (NumberOfCrabsMapInThisPosition, Maybe Fuel) -> Int
alignFoldF positionToAlign acc horizontalPosition (numberOfCrabsMapInThisPosition, _) =
    acc + (abs (positionToAlign - horizontalPosition) * numberOfCrabsMapInThisPosition)

countTotalFuelToAlign :: CrabsMap -> HorizontalPosition -> Int
countTotalFuelToAlign crabsMap positionToAlign = IntMap.foldlWithKey' (alignFoldF positionToAlign) 0 crabsMap


takeMinMaxPositions :: CrabsMap -> (HorizontalPosition, HorizontalPosition)
takeMinMaxPositions crabsMap =
    let keys = IntMap.keys crabsMap
    in (head keys, last keys)

getLeastFuelCostAlignPosition :: CrabsMap -> (HorizontalPosition, Int, String)
getLeastFuelCostAlignPosition crabsMap =
    let (minPos, maxPos) = takeMinMaxPositions crabsMap
    in foldl'
        (\ (horizPos, leastFuelToAlign, s) positionToCheck ->
            let burnedFuelToAlign = countTotalFuelToAlign crabsMap positionToCheck
            in if leastFuelToAlign > burnedFuelToAlign
                then (positionToCheck, burnedFuelToAlign, s ++ "burnedFuelToAlign " ++ show burnedFuelToAlign ++ " ")
                else (
                    horizPos,
                    leastFuelToAlign,
                    s ++ "leastFuelToAlign " ++ show leastFuelToAlign
                        ++ " is < than burnedFuelToAlign " ++ show burnedFuelToAlign
                        
                        ++ "; "
                )
        )
        (minPos, 1000000000, "")
        [minPos..maxPos]


getResultsWithoutDebug :: (HorizontalPosition, Int, String) -> (HorizontalPosition, Int)
getResultsWithoutDebug (horizontalPosition, totalFuelCost, _) = (horizontalPosition, totalFuelCost)



solveTest :: IO ()
solveTest = readFile "testInput"
    >>= print
        . getResultsWithoutDebug
        . getLeastFuelCostAlignPosition
        . parseInput

solve :: IO ()
solve = readFile "input.txt"
    >>= print
        . getResultsWithoutDebug
        . getLeastFuelCostAlignPosition
        . parseInput

solveTest2 :: IO ()
solveTest2 = readFile "testInput"
    >>= print
        . parseInput

solve2 :: IO ()
solve2 = readFile "input.txt"
    >>= print
        . parseInput

