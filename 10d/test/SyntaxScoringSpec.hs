module SyntaxScoringSpec where 

import Data.List (isPrefixOf)
import Test.Hspec (Spec, describe, it, runIO, shouldBe, shouldNotBe)

import SyntaxScoring

isLineIncomplete :: Maybe String -> Bool
isLineIncomplete (Just str) = isPrefixOf haveNoOpenOrClosingBracketIn str
isLineIncomplete _ = False

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"

    input <- runIO $ readFile "input.txt"

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

    describe "isLineCorruptedAndWhere" $ do
        it "reports error for no open or closing bracket case"
            $ shouldBe
                (isLineCorruptedAndWhere "{([(<{")
                (Just "Have no open or closing bracket in (\"{([(<{\",\"\")")

        it "reports error for mismatched parens"
            $ shouldBe
                (isLineCorruptedAndWhere "{([(<{]")
                (Just "Expected open bracket for ']', but found '{' instead.")

        it "works"
            $ shouldBe
                (isLineCorruptedAndWhere "{([(<{}[<>[]}>{[]{[(<()>")
                -- from puzzle text
                -- (Just "Expected ], but found } instead.")
                (Just "Expected open bracket for '}', but found '[' instead.")

        it "works for testInput"
            $ shouldBe
                (map (\maybeErrorString -> if isLineIncomplete maybeErrorString
                        then Nothing
                        else maybeErrorString
                    )
                    $ map isLineCorruptedAndWhere $ parseInput testInput
                )
                $ pure [
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

                (Just "Expected open bracket for '}', but found '[' instead.")
