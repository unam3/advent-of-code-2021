module TheTreacheryofWhales where


import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl')

{-
Main questions to answer:

1)•Determine the horizontal position that the crabs can align to using the least fuel possible.
    - brute force from 0 to 1991?
    - any analytical solutions?
        656 number of positions but there can be intermediate numbers between them
2) How much fuel must they spend to align to that position.
    

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


solveTest :: IO ()
solveTest = readFile "testInput"
    >>= print
        . parseInput

solve :: IO ()
solve = readFile "input.txt"
    >>= print
        . parseInput

solveTest2 :: IO ()
solveTest2 = readFile "testInput"
    >>= print
        . parseInput

solve2 :: IO ()
solve2 = readFile "input.txt"
    >>= print
        . parseInput

