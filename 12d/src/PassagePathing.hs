module PassagePathing where

import Data.Bifunctor (second)
import Data.Char (isUpper)
import Data.List (elemIndex, foldl', nub, union)
import Data.Map.Strict (Map, (!), empty, insert, insertWith, keys)
import Data.Maybe (isJust)
import Prelude hiding (map)


normalize :: (String, String) -> (String, String)
normalize (string, "start") = ("start", string)
normalize ("end", string) = (string, "end")
normalize tuple = tuple

type Relations = Map String [String]

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


type Path = [String]

areAllCapitals :: String -> Bool
areAllCapitals = all isUpper

areAllLower :: String -> Bool
areAllLower = not . areAllCapitals

constructPaths :: Path -> Relations -> [Path]
constructPaths paths@("end":_) _  = [paths]
constructPaths path relations  =
    let availableParts = relations ! (head path)
        -- if available part isAllLower and already in Path then we should discard it
        filterVisitedSmallCaves =
            filter
                (\ part -> not (areAllLower part && isJust (elemIndex part path)))
        connectedParts = (: path) <$> filterVisitedSmallCaves availableParts
    in foldl'
        (\ acc updatedPath ->
            if head updatedPath == "end" 
            then updatedPath : acc
            else (constructPaths updatedPath relations) ++ acc
        )
        []
        connectedParts




constructPathsWrapper :: Relations -> [Path]
-- always starts with "start"
constructPathsWrapper = fmap (fmap reverse) $ constructPaths ["start"]

getSmallCavesNames :: Relations -> [String]
getSmallCavesNames = filter (\ key -> key /= "start" && areAllLower key) . keys

modifyRelationsToVisitSmallCaveTwice :: Relations -> String -> Relations
modifyRelationsToVisitSmallCaveTwice relations smallCaveName =
    let twinName = smallCaveName ++ "second"
    -- is this map somehow relate to the required nub operation?
    in fmap 
        (\ keyToRelations ->
            if elem smallCaveName keyToRelations
            then twinName : keyToRelations
            else keyToRelations
        )
        $ insert
            twinName
            (relations ! smallCaveName)
            relations

fuseTwiceVisitedSmallCave :: String -> [Path] -> [Path]
fuseTwiceVisitedSmallCave twinName paths = nub
    $ fmap (
        fmap (\ caveName ->
            if caveName == twinName
            then take (length twinName - length "second") twinName
            else caveName
        )
    )
    paths

collectTwiceVisitResults :: Relations -> [Path]
collectTwiceVisitResults relations =
    let smallCavesNames = getSmallCavesNames relations
    in foldl'
        (\ acc smallCaveName ->
            let twiceVisitResults = fuseTwiceVisitedSmallCave
                    (smallCaveName ++ "second")
                    $ constructPathsWrapper
                    $ modifyRelationsToVisitSmallCaveTwice relations smallCaveName
            in union acc twiceVisitResults
        )
        []
        smallCavesNames
