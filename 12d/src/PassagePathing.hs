module PassagePathing where

import Data.Bifunctor (second)
import Data.List (foldl', union)
import Data.Map.Strict (Map, empty, insertWith)
import Prelude hiding (map)


normalize :: (String, String) -> (String, String)
normalize (string, "start") = ("start", string)
normalize ("end", string) = (string, "end")
normalize tuple = tuple

parseInput :: String -> Map String [String]
parseInput =
    foldl'
        (\ map (leftPart, rightPart) ->
            -- if one part is "start" then we only should add "start" to "x" relation
            -- if one part is "end" then we only should add "x" to "end" relation
            if leftPart == "start" || rightPart == "end"
            then insertWith union leftPart [rightPart] map
            -- otherwise add mutual relations
            else insertWith union rightPart [leftPart]
                $ insertWith union leftPart [rightPart] map
        )
        empty
        . fmap (
            normalize
                -- break each line by '-' into a tuple
                . (second tail . span (/= '-'))
        )
        . lines




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

