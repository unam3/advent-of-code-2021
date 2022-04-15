module TheTreacheryofWhales where


import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl')

{-
Main questions to answer:

1) Determine the horizontal position that the crabs can align to using the least fuel possible.
2) How much fuel must they spend to align to that position.

How represent input numbers?
How represent fuel-position values?

-}

type NumberOfCrabsMapInThisPosition = Int -- will be our multiplier
type Fuel = Int

-- key -- horizontal position
type CrabsMap = IntMap (NumberOfCrabsMapInThisPosition, Maybe Fuel)

-- How do we get list iof ints from the imput?
--  - concat square brackets and use readEither
--  - replace commas with whitespaces and use words, then read


type HorizontolPosition = Int

foldF :: CrabsMap -> HorizontolPosition -> CrabsMap
foldF crabsMap horizontolPosition =
    IntMap.insertWith
        (
            \ _ (oldNumberOfCrabsInThisPosition, oldMaybeFuel) -> (oldNumberOfCrabsInThisPosition + 1, oldMaybeFuel)
        )
        horizontolPosition
        (1, Nothing)
        crabsMap


parseInput :: String -> CrabsMap
parseInput inputString =
    let crabPositionsList = (read $ concat ["[", inputString, "]"] :: [HorizontolPosition])
    in foldl' foldF IntMap.empty crabPositionsList


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

