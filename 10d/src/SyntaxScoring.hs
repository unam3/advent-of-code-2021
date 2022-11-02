module SyntaxScoring where

import Data.List (isPrefixOf)


parseInput :: String -> [String]
parseInput = lines

-- 1. Find corrupted lines: lines with a chunk that closes with the wrong character

-- are incompleted lines ones with odd number of characters? (nope)

isLineCharactersNumberEven :: String -> Bool
isLineCharactersNumberEven = even . length

filterLinesWithOddNumberOfCharectersOut :: [String] -> [String]
filterLinesWithOddNumberOfCharectersOut = filter isLineCharactersNumberEven


isClosingChunkCharacter :: Char -> Bool
isClosingChunkCharacter ')' = True
isClosingChunkCharacter ']' = True
isClosingChunkCharacter '}' = True
isClosingChunkCharacter '>' = True
isClosingChunkCharacter _ = False

isOpenBracketMatchTo :: (Char, Char) -> Bool
isOpenBracketMatchTo ('(', ')') = True
isOpenBracketMatchTo ('[', ']') = True
isOpenBracketMatchTo ('{', '}') = True
isOpenBracketMatchTo ('<', '>') = True
isOpenBracketMatchTo (_, _) = False

haveNoOpenOrClosingBracketIn :: String
haveNoOpenOrClosingBracketIn = "Illegal line: have no open or closing bracket in "

isLineCorruptedOrIllegalAndWhere :: String -> Maybe String
isLineCorruptedOrIllegalAndWhere line =
    -- 1. Find first closing bracket from the left.
    let (openParenPart, closingParenPart) = break isClosingChunkCharacter line
    -- ("[({(<((", "))[]>[[{[]{<()<>>")
        leftParen = last openParenPart
        rightParen = head closingParenPart
        reducedLine = init openParenPart ++ tail closingParenPart

    -- 2. Compare it with open bracket to the left.
    in if (null openParenPart) || (null closingParenPart)
        then Just $ haveNoOpenOrClosingBracketIn ++ show (openParenPart, closingParenPart)

    -- 3. Report error or process rest of the input.
        else if isOpenBracketMatchTo (leftParen, rightParen)
            then isLineCorruptedOrIllegalAndWhere reducedLine
            else Just
                $ "Expected open bracket for " ++ show rightParen ++ ", but found " ++ show leftParen ++ " instead."


isLineIncomplete :: Maybe String -> Bool
isLineIncomplete (Just str) = isPrefixOf haveNoOpenOrClosingBracketIn str
isLineIncomplete _ = False

mapIncompleteLineToNothing :: Maybe String -> Maybe String
mapIncompleteLineToNothing maybeErrorString =
    if isLineIncomplete maybeErrorString
    then Nothing
    else maybeErrorString

collectCorruptedLines' :: Maybe String -> [String] -> [String]
collectCorruptedLines' (Just line) acc = line : acc
collectCorruptedLines' _ acc = acc

collectCorruptedLines :: [Maybe String] -> [String]
collectCorruptedLines = foldr collectCorruptedLines' [] . fmap mapIncompleteLineToNothing


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

