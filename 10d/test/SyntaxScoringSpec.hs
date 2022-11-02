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
                (Just $ haveNoOpenOrClosingBracketIn ++ "(\"{([(<{\",\"\")")

        it "reports error for mismatched parens"
            $ shouldBe
                (isLineCorruptedOrIllegalAndWhere "{([(<{]")
                (Just "Expected open bracket for ']', but found '{' instead.")

        it "works"
            $ shouldBe
                (isLineCorruptedOrIllegalAndWhere "{([(<{}[<>[]}>{[]{[(<()>")
                -- from puzzle text
                -- (Just "Expected ], but found } instead.")
                (Just "Expected open bracket for '}', but found '[' instead.")

        it "works for testInput"
            $ shouldBe
                (fmap mapIncompleteLineToNothing
                    $ fmap isLineCorruptedOrIllegalAndWhere $ parseInput testInput
                )
                [
                    Nothing,
                    Nothing,
                    Just "Expected open bracket for '}', but found '[' instead.",
                    Nothing,
                    Just "Expected open bracket for ')', but found '[' instead.",
                    Just "Expected open bracket for ']', but found '(' instead.",
                    Nothing,
                    Just "Expected open bracket for ')', but found '<' instead.",
                    Just "Expected open bracket for '>', but found '[' instead.",
                    Nothing
                ]

    describe "find corrupted lines" $ do
        it "works for testInput"
            $ shouldBe
                (collectCorruptedLines
                    -- [String] -> [Maybe String]
                    $ fmap isLineCorruptedOrIllegalAndWhere
                    -- String -> [String]
                    $ parseInput testInput
                )
                [
                    "Expected open bracket for '}', but found '[' instead.",
                    "Expected open bracket for ')', but found '[' instead.",
                    "Expected open bracket for ']', but found '(' instead.",
                    "Expected open bracket for ')', but found '<' instead.",
                    "Expected open bracket for '>', but found '[' instead."
                ]
