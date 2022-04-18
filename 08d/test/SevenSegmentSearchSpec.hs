module SevenSegmentSearchSpec where 

import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import SevenSegmentSearch


tenUniquePatterns = words $ normalize "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab"

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

    describe "get1" $ do
        it "works"
            $ shouldBe
                (get1 tenUniquePatterns)
                "ab"

    describe "get4" $ do
        it "works"
            $ shouldBe
                (get4 tenUniquePatterns)
                "abef"

    describe "get7" $ do
        it "works"
            $ shouldBe
                (get7 tenUniquePatterns)
                "abd"

    describe "get8" $ do
        it "works"
            $ shouldBe
                (get8 tenUniquePatterns)
                "abcdefg"

    describe "derive6From8And1" $ do
        it "works"
            $ shouldBe
                (derive6From8And1 (get8 tenUniquePatterns) (get1 tenUniquePatterns) tenUniquePatterns)
                "6 definition is bcdefg"

    describe "deriveBLAndBFrom147" $ do
        it "works"
            $ shouldBe
                (deriveBLAndBFrom147 "be" "fbe" "gcbe")
                "'ad' are the either left bottom or bottom segments of the seven-digit display"
