module SevenSegmentSearchSpec where 

import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import SevenSegmentSearch

spec :: Spec
spec = do
    describe "parseInput" $ do
        it "works"
            $ shouldBe
                (parseInput "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")
                [["cdfeb", "fcadb", "cdfeb", "cdbaf"]]

    describe "collectAppearenceOf1478" $ do
        it "works"
            $ shouldBe
                (collectAppearenceOf1478 ["c", "ab", "f", "c", "abcdefg", "f", "c", "baf", "f", "c", "abce", "f"])
                ["ab", "abcdefg", "baf", "abce"]


    testInput <- runIO $ readFile "testInput"

    input <- runIO $ readFile "input.txt"

    describe "countAppearenceOf1478" $ do
        it "works for testInput"
            $ shouldBe
                (countAppearenceOf1478 $ parseInput testInput)
                26

        it "works for input.txt"
            $ shouldBe
                (countAppearenceOf1478 $ parseInput input)
                495

    describe "normalize" $ do
        it "works"
            $ shouldBe
                (normalize "be cfbegad cbdgef")
                "be abcdefg bcdefg"

    describe "deriveTopFrom1And7" $ do
        it "works"
            $ shouldBe
                (deriveTopFrom1And7 "be" "fbe")
                "'\"f\"' is the top segment"

    describe "deriveBLAndBFrom147" $ do
        it "works"
            $ shouldBe
                (deriveBLAndBFrom147 "be" "fbe" "gcbe")
                "'ad' are the either left bottom or bottom segments of the seven-digit display"
