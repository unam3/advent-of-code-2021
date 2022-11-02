module SyntaxScoringSpec where 

import Test.Hspec (Spec, describe, it, runIO, shouldBe, shouldNotBe)

import SyntaxScoring

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"

    --input <- runIO $ readFile "input.txt"

    describe "criteria for incomplete lines" $ do
        it "is not right"
            $ shouldNotBe
                (filterLinesWithOddNumberOfCharectersOut $ parseInput testInput)
                [
                    "{([(<{}[<>[]}>{[]{[(<()>",
                    "[[<[([]))<([[{}[[()]]]",
                    "[{[{({}]{}}([{[{{{}}([]",
                    "[<(<(<(<{}))><([]([]()",
                    "<{([([[(<>()){}]>(<<{{"
                ]

    describe "isLineCorruptedOrIllegalAndWhere" $ do
        it "reports error for no open or closing bracket case"
            $ shouldBe
                (isLineCorruptedOrIllegalAndWhere "{([(<{")
                (Left $ haveNoOpenOrClosingBracketIn ++ "(\"{([(<{\",\"\")")

        it "reports error for mismatched parens"
            $ shouldBe
                (isLineCorruptedOrIllegalAndWhere "{([(<{]")
                (Right ("Expected open bracket for ']', but found '{' instead.", ']'))

        it "works"
            $ shouldBe
                (isLineCorruptedOrIllegalAndWhere "{([(<{}[<>[]}>{[]{[(<()>")
                -- from puzzle text
                -- (Just "Expected ], but found } instead.")
                (Right ("Expected open bracket for '}', but found '[' instead.", '}'))

        it "works for testInput"
            $ shouldBe
                (fmap isLineCorruptedOrIllegalAndWhere $ parseInput testInput)
                [
                    Left "Illegal line: have no open or closing bracket in (\"[({([[{{\",\"\")",
                    Left "Illegal line: have no open or closing bracket in (\"({[<{(\",\"\")",
                    Right ("Expected open bracket for '}', but found '[' instead.", '}'),
                    Left "Illegal line: have no open or closing bracket in (\"((((<{<{{\",\"\")",
                    Right ("Expected open bracket for ')', but found '[' instead.", ')'),
                    Right ("Expected open bracket for ']', but found '(' instead.", ']'),
                    Left "Illegal line: have no open or closing bracket in (\"<{[{[{{[[\",\"\")",
                    Right ("Expected open bracket for ')', but found '<' instead.", ')'),
                    Right ("Expected open bracket for '>', but found '[' instead.", '>'),
                    Left "Illegal line: have no open or closing bracket in (\"<{([\",\"\")"
                ]

    describe "find corrupted lines" $ do
        it "works for testInput"
            $ shouldBe
                (collectCorruptedLinesChar
                    -- [String] -> [Maybe String]
                    $ fmap isLineCorruptedOrIllegalAndWhere
                    -- String -> [String]
                    $ parseInput testInput
                )
                [
                    '}',
                    ')',
                    ']',
                    ')',
                    '>'
                ]

    describe "getTotalSyntaxErrorScore" $ do
        it "works for testInput"
            $ shouldBe
                (getTotalSyntaxErrorScore
                    $ collectCorruptedLinesChar
                    -- [String] -> [Maybe String]
                    $ fmap isLineCorruptedOrIllegalAndWhere
                    -- String -> [String]
                    $ parseInput testInput
                )
                26397
