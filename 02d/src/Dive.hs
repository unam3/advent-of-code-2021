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

solve :: IO ()
solve = readFile "input.txt"
    >>= print
        . uncurry (*)
        . calculate
        . parseInput


type Aim = Int
type State1 = (HorizontalPosition, Depth, Aim)

calculate1' :: State1 -> Command -> State1
calculate1' (horizontalPosition, depth, aim) (Forward value) = (horizontalPosition + value, depth + (aim * value), aim)
calculate1' (horizontalPosition, depth, aim) (Down value) = (horizontalPosition, depth, aim + value)
calculate1' (horizontalPosition, depth, aim) (Up value) = (horizontalPosition, depth, aim - value)

calculate1 :: [Command] -> State1
calculate1 =
    let initialState = (0, 0, 0)
    in foldl calculate1' initialState

multiplyHorizontalPositionAndDepth :: State1 -> Int
multiplyHorizontalPositionAndDepth (horizontalPosition, depth, _) = horizontalPosition * depth

solveTest2 :: IO ()
solveTest2 = readFile "testInput"
    >>= print
        . multiplyHorizontalPositionAndDepth
        . calculate1
        . parseInput

solve2 :: IO ()
solve2 = readFile "input.txt"
    >>= print
        . multiplyHorizontalPositionAndDepth
        . calculate1
        . parseInput

