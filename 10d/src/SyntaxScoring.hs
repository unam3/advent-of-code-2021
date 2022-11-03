module SyntaxScoring where

import Data.Either (isLeft)
import Data.List (foldl')

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


type IllegalOrCorrupted = Either (String, String) (String, Char)

haveNoOpenOrClosingBracketIn :: String
haveNoOpenOrClosingBracketIn = "Illegal line: have no open or closing bracket in "

isLineCorruptedOrIllegalAndWhere :: String -> IllegalOrCorrupted
isLineCorruptedOrIllegalAndWhere line =
    -- 1. Find first closing bracket from the left.
    let (openParenPart, closingParenPart) = break isClosingChunkCharacter line
    -- ("[({(<((", "))[]>[[{[]{<()<>>")
        leftParen = last openParenPart
        rightParen = head closingParenPart
        reducedLine = init openParenPart ++ tail closingParenPart

    -- 2. Compare it with open bracket to the left.
    in if null openParenPart || null closingParenPart
        then Left (
            haveNoOpenOrClosingBracketIn ++ show (openParenPart, closingParenPart),
            openParenPart
        )

    -- 3. Report error or process rest of the input.
        else if isOpenBracketMatchTo (leftParen, rightParen)
            then isLineCorruptedOrIllegalAndWhere reducedLine
            else Right
                (
                    "Expected open bracket for " ++ show rightParen ++ ", but found " ++ show leftParen ++ " instead.",
                    rightParen
                )


collectCorruptedLinesChar' :: IllegalOrCorrupted -> [Char] -> [Char]
collectCorruptedLinesChar' (Right (_, char)) acc = char : acc
collectCorruptedLinesChar' _ acc = acc

collectCorruptedLinesChar :: [IllegalOrCorrupted] -> [Char]
collectCorruptedLinesChar = foldr collectCorruptedLinesChar' []


getScorePointsForIllegalCharacter :: Char -> Integer
getScorePointsForIllegalCharacter ')' = 3
getScorePointsForIllegalCharacter ']' = 57
getScorePointsForIllegalCharacter '}' = 1197
getScorePointsForIllegalCharacter '>' = 25137
getScorePointsForIllegalCharacter _ = error "No such character to lookup."

getTotalSyntaxErrorScore :: [Char] -> Integer
getTotalSyntaxErrorScore = foldl' (\acc char -> acc + getScorePointsForIllegalCharacter char) 0

solve :: IO ()
solve = readFile "input.txt"
    >>= print
        . getTotalSyntaxErrorScore
        . collectCorruptedLinesChar
        . fmap isLineCorruptedOrIllegalAndWhere
        . parseInput



getIncompleteLines :: [String] -> [(String, IllegalOrCorrupted)]
getIncompleteLines inputLines =
    let illegalOrCorruptedList = isLineCorruptedOrIllegalAndWhere <$> inputLines
    in filter (isLeft . snd) $ zip inputLines illegalOrCorruptedList


solveTest2 :: IO ()
solveTest2 = readFile "testInput"
    >>= print
        . parseInput

solve2 :: IO ()
solve2 = readFile "input.txt"
    >>= print
        . parseInput

