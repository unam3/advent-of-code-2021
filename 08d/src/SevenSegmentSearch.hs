module SevenSegmentSearch where


import Data.List (elemIndex, foldl', sort, union, (\\))

-- In the output values, how many times do digits 1, 4, 7, or 8 appear?

{-

1) Extract 4 digits after vertical bar "|"
2) count how many times one of four number (1,4,7,8) are occured in all exctracted lines

-}


removeBarSpace :: String -> String
removeBarSpace = drop 2

extractOtputValue :: String -> [String]
extractOtputValue = words . removeBarSpace . dropWhile (/= '|')

parseInput :: String -> [[String]]
parseInput = fmap extractOtputValue . lines

collectAppearenceOf1478 :: [String] -> [String]
collectAppearenceOf1478 =
    foldl'
        (\ acc digitRepresentation ->
            let len = length digitRepresentation
            {-
                only digit "1" is rendered for seven-segment display with 2 segments
                4 - 4
                7 - 3
                8 - 7
            -}
            in if len == 2 || len == 3 || len == 4 || len == 7
                then acc ++ [digitRepresentation]
                else acc
        )
        []

countAppearenceOf1478 :: [[String]] -> Int
countAppearenceOf1478 = sum . fmap (length . collectAppearenceOf1478)


solveTest :: IO ()
solveTest = readFile "testInput"
    >>= print
        . countAppearenceOf1478
        . parseInput

solve :: IO ()
solve = readFile "input.txt"
    >>= print
        . parseInput


-- input has different order of signal wires 10 patterns and output value: "cabfe" and "febac" for example
normalize :: String -> String
normalize = unwords . fmap sort . words

splitUniquePatternsAndOutput :: String -> (String, String)
splitUniquePatternsAndOutput inputEntry =
    let (uniquePatternsAndSpace, outputWithBarAndSpace) = span (/= '|') inputEntry
        -- removes last space in string; subject of optimisation
        uniquePatterns = unwords $ words uniquePatternsAndSpace
        output = removeBarSpace outputWithBarAndSpace
    in (uniquePatterns, output)

p2ParseInput :: String -> [(String, String)]
p2ParseInput = fmap (splitUniquePatternsAndOutput . normalize) . lines

{-

Deduce four digits of the output value and decode them.

1) identify T (top) segment representation:
    if we have 1 ab and 7 abc then top is "c"

2) get 9 from (1,4,7):
    if we have 1 ab and 4 abcd then top left and middle are either "c" or "d"
        and this is useful for deducing 5, 6, 8 and 9

3) derive bottom from 9

4) deduce 0

5) from 0 derive borrom left BL and middle M

6) make 3 from 9


if we have 1, 4 and 7 we need only one side for 9

n    number of lines
1 - 2 exclusively
7 - 3 exclusively
4 - 4 exclusively
2 - 5
3 - 5
5 - 5
6 - 6
0 - 6
9 - 6
8 - 7 exclusively

1 (TR, BR)          is in 0, 1, 3, 4, 7, 8, 9
4 (TR, BR, M, TL)   is in 4, 8, 9
7 (T, TR, BR)       is in 0, 3, 7, 8, 9

If we have (BL, B), (TL, M) how we can know for sure which is which?


Let's examine 6: it hadn't only TR segment and we can find it by excluding parts of 1 from 8 and analyze all ten unique representations: which one hasn't segment of 1 will be the 6. Also by doing that we can identify TR and BR segments

5 as 6 is has no TR segment, so we can identify B and then BL.

-}


getFixedLengthDefinition :: Int -> [String] -> String
getFixedLengthDefinition definitionLength = head . filter ((== definitionLength) . length)

get1 :: [String] -> String
get1 = getFixedLengthDefinition 2

get4 :: [String] -> String
get4 = getFixedLengthDefinition 4

get7 :: [String] -> String
get7 = getFixedLengthDefinition 3

get8 :: [String] -> String
get8 = getFixedLengthDefinition 7

derive6From8And1 :: String -> String -> [String] -> String
--   the fuck eight is [o, o1] and why? possibly a bug here
derive6From8And1 eight [o, o1] uniquePatterns =
    let eitherSix = eight \\ (show o)
        orSix = eight \\ (show o1)
    in case elemIndex eitherSix uniquePatterns of
        Just _ -> eitherSix
        _ -> orSix
derive6From8And1 _ wrongInput _ = error "wrong input for 'one': " ++ wrongInput

identifyTRAndBRSegments :: String -> String -> String -> (String, String)
identifyTRAndBRSegments eight six one =
    let topRightSegment = eight \\ six
        bottomRightSegment = one \\ topRightSegment
    in (topRightSegment, bottomRightSegment)


hasWord :: Char -> String -> Bool
hasWord char string = (>= 1) . length $ filter (== char) string

derive5From6AndTopRight :: String -> String -> [String] -> String
derive5From6AndTopRight six topRightSegment uniquePatterns =
    let digitsWithoutTRSegment =
            filter
                (not . hasWord (head topRightSegment))
                uniquePatterns
    in head $ digitsWithoutTRSegment \\ [six]

identifyBLSegment :: String -> String -> String
identifyBLSegment six five = six \\ five

-- not used anywhere?
identifyBSegment :: String -> String -> String -> String -> String
identifyBSegment seven four eight bLSegment =
    let bottomLeftAndBottomSegments = eight \\ (union seven four)
    in bottomLeftAndBottomSegments \\ show bLSegment


derive9From8AndBottomLeft :: String -> String -> String
derive9From8AndBottomLeft eight bottomLeftSegment = eight \\ bottomLeftSegment


getZeroTwoThree :: [String] -> [String]
getZeroTwoThree uniquePatterns = 
    let one = get1 uniquePatterns
        eight = get8 uniquePatterns
        six = derive6From8And1 eight one uniquePatterns
        topRightSegment = fst $ identifyTRAndBRSegments eight six one
        five = derive5From6AndTopRight six topRightSegment uniquePatterns
    in uniquePatterns \\ [
        one,
        (get4 uniquePatterns),
        five,
        six,
        (get7 uniquePatterns),
        eight,
        (derive9From8AndBottomLeft eight (identifyBLSegment six five))
    ]


derive3 :: [String] -> String
derive3 uniquePatterns =
    let one = get1 uniquePatterns
        eight = get8 uniquePatterns
        six = derive6From8And1 eight one uniquePatterns
        topRightSegment = fst $ identifyTRAndBRSegments eight six one
        five = derive5From6AndTopRight six topRightSegment uniquePatterns
        bottomLeftSegment = identifyBLSegment six five
        zeroTwoThree = getZeroTwoThree uniquePatterns
    in head $ filter (not . hasWord (head bottomLeftSegment)) zeroTwoThree

derive0And2 :: String -> String -> String -> [String] -> (String, String)
derive0And2 five three topRightSegment uniquePatterns =
    let topLeftSegment = (five \\ three) \\ topRightSegment
        zeroTwoThree = getZeroTwoThree uniquePatterns
        zeroTwo = zeroTwoThree \\ [three]
        zero = concat $ filter (hasWord $ head topLeftSegment) zeroTwo
        two = concat $ zeroTwo \\ [zero]
    in (zero, two)

-- only 3 digits have no middle segment: 0, 1, 7
--derive0 :: String -> Char -> [String] -> String
--derive0 six topRightSegment uniquePatterns =
--    let digitsWithoutTRSegment =
--            filter
--                (not . hasWord topRightSegment)
--                uniquePatterns
--        fiveRepresentation = digitsWithoutTRSegment \\ [six]
--    in "5 representation is " ++ concat fiveRepresentation

type DecodedDigits = [(String, Char)]

--decodeFourDigitOuput :: DecodedDigits -> [String] -> [String] -> DecodedDigits
--decodeFourDigitOuput decodedDigits uniquePatterns fourDigitRepresentation =
    -- check if any digits need to decode
    --  any fourDigitRepresentation elements must not be in decodedDigits
    --if any (\ digitRepresentation -> isNothing $ lookup digitRepresentation decodedDigits)
    --    then []
    --    else [("666", 666)]

    -- check if any of digits are easily identifiable (1,4,7,8)
    
    -- deduce and identify only necessary digits

deduceDigitRepresentations :: [String] -> DecodedDigits
deduceDigitRepresentations uniquePatterns =
    let one = get1 uniquePatterns
        four = get4 uniquePatterns
        seven = get7 uniquePatterns
        eight = get8 uniquePatterns
        six = derive6From8And1 eight one uniquePatterns
        (topRightSegment, _) = identifyTRAndBRSegments eight six one
        five = derive5From6AndTopRight six topRightSegment uniquePatterns
        bottomLeftSegment = identifyBLSegment six five
        nine = derive9From8AndBottomLeft eight bottomLeftSegment
        three = derive3 uniquePatterns
        (zero, two) = derive0And2 five three topRightSegment uniquePatterns
    in [
        (one, '1'),
        (four, '4'),
        (seven, '7'),
        (eight, '8'),
        (six, '6'),
        (five, '5'),
        (nine, '9'),
        (three, '3'),
        (zero, '0'),
        (two, '2')
    ]

foldF :: Show a => Maybe [a] -> Maybe a -> Maybe [a]
foldF (Just acc) (Just el) = Just (acc ++ [el])
foldF acc el = error $ "wrong args: " ++ show (acc, el)

decodeFourDigitOutput :: (String, String) -> Int
decodeFourDigitOutput (uniquePatterns, output) =
    let digitRepresentations = deduceDigitRepresentations $ words uniquePatterns
        maybeStringNumber = foldl'
            foldF
            (Just "")
            $ fmap (`lookup` digitRepresentations) (words output)
    in case maybeStringNumber of
        (Just stringNumber) -> read stringNumber
        _ -> error "Something went wrong™"


solveTest2 :: IO ()
solveTest2 = readFile "testInput"
    >>= print
        . sum
        . fmap decodeFourDigitOutput
        . p2ParseInput

solve2 :: IO ()
solve2 = readFile "input.txt"
    >>= print
        . sum
        . fmap decodeFourDigitOutput
        . p2ParseInput
