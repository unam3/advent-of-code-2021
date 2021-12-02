module Dive where


data Command = Forward Int | Down Int | Up Int
    deriving Show

parseCommand :: [String] -> Command
parseCommand ["forward", commandValue] = Forward $ read commandValue
parseCommand ["down", commandValue] = Down $ read commandValue
parseCommand ["up", commandValue] = Up $ read commandValue
parseCommand nonMatchedInput = error ("non matched input: " ++ show nonMatchedInput)

parseInput :: String -> [Command]
parseInput = fmap (parseCommand . words) . lines

type HorizontalPosition = Int
type Depth = Int
type State = (HorizontalPosition, Depth)

calculate' :: State -> Command -> State
calculate' (horizontalPosition, depth) (Forward value) = (horizontalPosition + value, depth)
calculate' (horizontalPosition, depth) (Down value) = (horizontalPosition, depth + value)
calculate' (horizontalPosition, depth) (Up value) = (horizontalPosition, depth - value)

calculate :: [Command] -> State
calculate =
    let initialState = (0, 0)
    in foldl calculate' initialState

solveTest :: IO ()
solveTest = readFile "testInput"
    >>= print
        . uncurry (*)
        . calculate
        . parseInput

solveTest2 :: IO ()
solveTest2 = readFile "testInput"
    >>= print
        . parseInput

solve :: IO ()
solve = readFile "input.txt"
    >>= print
        . uncurry (*)
        . calculate
        . parseInput

solve2 :: IO ()
solve2 = readFile "input.txt"
    >>= print
        . parseInput

