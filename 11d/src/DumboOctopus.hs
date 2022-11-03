module DumboOctopus where

--import Data.Maybe (catMaybes)
--import Text.Read (readMaybe)
import Data.Map.Strict (Map, fromList)

charToInt :: Char -> Int
charToInt = read . (: [])

parseInput :: String -> Map (Int, Int) Int
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
