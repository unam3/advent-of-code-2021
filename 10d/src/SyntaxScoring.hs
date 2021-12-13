module SyntaxScoring where

import Debug.Trace (trace)

{-

1 take only corrupted lines from input
2 find the first illegal character in each corrupted line
3 look it up in the following table
4 count total syntax error score for those errors

-}

type Line = String

parseInput :: String -> [Line]
parseInput = lines


isNumberOfCharsEven :: Line -> Bool
isNumberOfCharsEven = even . length


type Chunk = String

getLineChunks :: Line -> Chunk
getLineChunks line@(_:_:_) = 
    let tail' = tail line
        chunks = take (length tail' - 1) tail'
    in chunks
getLineChunks nonMatchedLine =  error "wrong input: " ++ nonMatchedLine


getClosingBracket :: Char -> Char
getClosingBracket '(' = ')'
getClosingBracket '[' = ']'
getClosingBracket '{' = '}'
getClosingBracket '<' = '>'
getClosingBracket nonMatchedChar = error $ "non matched char" ++ show nonMatchedChar

getOpenBracket :: Char -> Char
getOpenBracket ')' = '('
getOpenBracket ']' = '['
getOpenBracket '}' = '{'
getOpenBracket '>' = '<'
getOpenBracket nonMatchedChar = error $ "non matched char" ++ show nonMatchedChar

isClosingBracket :: Char -> Bool
isClosingBracket ')' = True
isClosingBracket ']' = True
isClosingBracket '}' = True
isClosingBracket '>' = True
isClosingBracket _ = False

isOpenBracket :: Char -> Bool
isOpenBracket '(' = True
isOpenBracket '[' = True
isOpenBracket '{' = True
isOpenBracket '<' = True
isOpenBracket _ = False


data State = Proceed | Corrupted String | Illegal String
    deriving (Eq, Show)

getCorruptedChunk :: Chunk -> State

-- illegal
getCorruptedChunk input@(_ : []) = Illegal $ "illegal input: " ++ input

getCorruptedChunk "()" = Proceed
getCorruptedChunk "[]" = Proceed
getCorruptedChunk "{}" = Proceed
getCorruptedChunk "<>" = Proceed

-- illegal
getCorruptedChunk input@(')':_) = Illegal $ "illegal input: " ++ input
getCorruptedChunk input@(']':_) = Illegal $ "illegal input: " ++ input
getCorruptedChunk input@('}':_) = Illegal $ "illegal input: " ++ input
getCorruptedChunk input@('>':_) = Illegal $ "illegal input: " ++ input

getCorruptedChunk (bracket:chunkRest) =

    --trace  ("tuple: " ++ (show $ break isClosingBracket chunkRest)) $
    case break isClosingBracket chunkRest of

        ([], []) -> Illegal $ "illegal input: " ++ (bracket:chunkRest)

        (_, []) -> Illegal $ "illegal input: " ++ (bracket:chunkRest)

        --([], rightPart) ->
        --    case getCorruptedChunk [bracket, head rightPart] of
        --        Nothing -> getCorruptedChunk $ tail rightPart
        --        -- ??
        --        justPluh -> justPluh

        --(leftPart@(_:_), [c]) ->
        --    case getCorruptedChunk [last leftPart, c] of
        --        -- illegal
        --        Nothing ->  Left $ "illegal input: " ++ (bracket:chunkRest)
        --        -- ??
        --        justPluh -> justPluh

        --(leftPart@(_:_), rightPart@(_:_:_)) ->
        --    case getCorruptedChunk [last leftPart, head rightPart] of
        --        Nothing -> getCorruptedChunk (bracket : init leftPart ++ tail rightPart)
        --        -- ??
        --        justPluh -> justPluh

        nonMatchedTuple -> Illegal $ "non matched tuple: " ++ show nonMatchedTuple

getCorruptedChunk [] = Illegal "illegal input: []"


--isLineWithCurruptedChunks :: Line -> Bool
--isLineWithCurruptedChunks = not . getCorruptedChunk

-- implement getFistIllegalCharecter :: Chunk -> Maybe Char based on getCorruptedChunk


solveTest :: IO ()
solveTest = readFile "testInput"
    >>= print
        . fmap (\ l -> (l, getCorruptedChunk l))
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

