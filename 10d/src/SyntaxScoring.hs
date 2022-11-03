module SyntaxScoring where

import Data.Either (isLeft, lefts, rights)
import Data.List (foldl', sort)

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

getJustIncompleteLines :: [(String, IllegalOrCorrupted)] -> [String]
getJustIncompleteLines =
    fmap snd
        . lefts
        . fmap snd


getChunkClosingCharacter :: Char -> Char
getChunkClosingCharacter '(' = ')'
getChunkClosingCharacter '[' = ']'
getChunkClosingCharacter '{' = '}'
getChunkClosingCharacter '<' = '>'
getChunkClosingCharacter char = error $ "no opening character for" ++ show char

getSequenceOfClosingCharacters' :: Either String (String, String) -> Either String (String, String)
getSequenceOfClosingCharacters' (Right (string@(_:_), closingCharactersAcc)) =
    let (openParenPart, closingParenPart) = break isClosingChunkCharacter string
    -- ("[({(<((", "))[]>[[{[]{<()<>>")
        leftParen = last openParenPart
        rightParen = head closingParenPart
        reducedLine = init openParenPart ++ tail closingParenPart

    --in if null openParenPart
    in if null closingParenPart
        then getSequenceOfClosingCharacters'
                $ Right (
                    init openParenPart,
                    closingCharactersAcc ++ [getChunkClosingCharacter leftParen]
                )

        else if isOpenBracketMatchTo (leftParen, rightParen)
            then getSequenceOfClosingCharacters'
                    $ Right (
                        reducedLine,
                        closingCharactersAcc
                    )
            else Left $ "Corrupted input string has no open chunk character match: " ++ string

getSequenceOfClosingCharacters' notNonEmptyRightList = notNonEmptyRightList

getSequenceOfClosingCharacters :: String -> Either String (String, String)
getSequenceOfClosingCharacters line = getSequenceOfClosingCharacters' $ Right (line, [])


getPointValue :: Char -> Int
getPointValue ')' = 1
getPointValue ']' = 2
getPointValue '}' = 3
getPointValue '>' = 4
getPointValue unexpectedChar = error $ "has unexpected input char" ++ [unexpectedChar]

getTotalScore' :: Int -> String -> Int
getTotalScore' =
    foldl (\ acc' char -> acc' * 5 + getPointValue char)

getTotalScore :: String -> Int
getTotalScore = getTotalScore' 0

getMiddleScore :: [Int] -> Int
getMiddleScore scores =
    let middleScoreIndex = quot (length scores) 2
    in scores !! middleScoreIndex


solvePart2 :: String -> Int
solvePart2 = 
    getMiddleScore
        . sort
        . fmap (getTotalScore . snd)
        . rights
        . fmap getSequenceOfClosingCharacters
        . getJustIncompleteLines
        . getIncompleteLines
        . parseInput
