module PassagePathing where

import Data.Bifunctor (second)
import Data.Char (isUpper)
import Data.List (elemIndex, foldl', union)
import Data.Map.Strict (Map, (!), empty, filterWithKey, insertWith)
import Data.Maybe (isJust)
import Prelude hiding (map)


normalize :: (String, String) -> (String, String)
normalize (string, "start") = ("start", string)
normalize ("end", string) = (string, "end")
normalize tuple = tuple

type Relations = Map String [String]

parseInput :: String -> Map String [String]
parseInput =
    --filterWithKey
    --    (\ k v ->
    --        not (
    --            areAllLower k && length v == 1
    --            && k /= "start"
    --            && v /= ["end"]
    --        )
    --    )
    --    . foldl'
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


type Path = [String]

areAllCapitals :: String -> Bool
areAllCapitals = all isUpper

areAllLower :: String -> Bool
areAllLower = not . areAllCapitals

constructPaths :: Path -> Relations -> (Path, [Path])
constructPaths path relations  =
    let availableParts = relations ! (head path)
        -- if available part isAllLower and already in Path then we should discard it
        filterVisitedSmallCaves =
            filter
                (\ part -> not (areAllLower part && (isJust $ elemIndex part path)))
        connectedParts = fmap (\ part -> part : path) $ filterVisitedSmallCaves availableParts
    in (path, connectedParts)


--    in fmap
--        (\ updatedPath ->
--            if head updatedPath == "end" 
--            then updatedPath
--            else concat $ constructPaths updatedPath relations
--        )
--        connectedParts


constructPathsWrapper :: Relations -> (Path, [Path])
-- always starts with "start"
constructPathsWrapper = constructPaths ["start"]

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

