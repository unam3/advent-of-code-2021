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


isLineLegal :: Line -> Bool
isLineLegal line@(_:_:_) = head line == last line
isLineLegal line = error $ "illegal input: " ++ line


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


--isClosingBracket = any (==) ")]}>"

isClosingBracket :: Char -> Bool
isClosingBracket ')' = True
isClosingBracket ']' = True
isClosingBracket '}' = True
isClosingBracket '>' = True
isClosingBracket _ = False

areChunksLegal :: Chunk -> Bool
areChunksLegal [_] = False
areChunksLegal [bracket, closingBracket] = isClosingBracket closingBracket && bracket == (getOpenBracket closingBracket)
areChunksLegal (bracket:chunkRest) =
    let closingBracket = getClosingBracket bracket
        -- find first closing bracket inside chunkRest
        --  "{[]}…>" -> ("{[", "]}…>")
        (leftPartWithOpenBracket, rightPartWithLeadingClosingBracket) = break isClosingBracket chunkRest
    -- check redundancy of this case after implementation
    in trace (show (bracket, chunkRest)) $
       trace (show (leftPartWithOpenBracket, rightPartWithLeadingClosingBracket)) $
        if leftPartWithOpenBracket == [getOpenBracket $ head rightPartWithLeadingClosingBracket] && (rightPartWithLeadingClosingBracket == [getClosingBracket (head rightPartWithLeadingClosingBracket), closingBracket])
        then True
        else if
            (not $ null leftPartWithOpenBracket)
            && last leftPartWithOpenBracket == (getOpenBracket $ head rightPartWithLeadingClosingBracket)
                then trace (show (init leftPartWithOpenBracket, tail rightPartWithLeadingClosingBracket))$
                    -- ("{[", "]}…>") -> ("{", "}…>")
                    -- get leftPart without ending char
                    -- get rightPart without leading char
                    let reducedChunks = bracket : (init leftPartWithOpenBracket)
                            ++ (tail rightPartWithLeadingClosingBracket)
                    in trace reducedChunks $ 
                        areChunksLegal reducedChunks
                else False
areChunksLegal [] = True


isLineCurrupted :: Line -> Bool
isLineCurrupted line =
    isNumberOfCharsEven line && isLineLegal line

-- implement getFistIllegalCharecter :: Chunk -> Maybe Char based on areChunksLegal


solveTest :: IO ()
solveTest = readFile "testInput"
    >>= print
        . parseInput

solve :: IO ()
solve = readFile "input.txt"
    >>= print
        . (\ lines' -> (length lines', length (filter isLineCurrupted lines')))
        . parseInput

solveTest2 :: IO ()
solveTest2 = readFile "testInput"
    >>= print
        . parseInput

solve2 :: IO ()
solve2 = readFile "input.txt"
    >>= print
        . parseInput

