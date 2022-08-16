module SevenSegmentSearchSpec where 

import Data.List (sort)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import SevenSegmentSearch


uniquePatterns :: [String]
uniquePatterns = words $ normalize "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab"

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
                (get1 uniquePatterns)
                "ab"

    describe "get4" $ do
        it "works"
            $ shouldBe
                (get4 uniquePatterns)
                "abef"

    describe "get7" $ do
        it "works"
            $ shouldBe
                (get7 uniquePatterns)
                "abd"

    describe "get8" $ do
        it "works"
            $ shouldBe
                (get8 uniquePatterns)
                "abcdefg"

    describe "derive6From8And1" $ do
        it "works"
            $ shouldBe
                (derive6From8And1 (get8 uniquePatterns) (get1 uniquePatterns) uniquePatterns)
                "bcdefg"

    describe "identifyTRAndBRSegments" $ do
        it "works"
            $ shouldBe
                (identifyTRAndBRSegments (get8 uniquePatterns) "bcdefg" (get1 uniquePatterns))
                ("a", "b")

    describe "hasWord" $ do
        it "works"
            $ shouldBe
                (hasWord 'a' "bca")
                True

        it "doesn't have false positive"
            $ shouldBe
                (hasWord 'a' "bc")
                False
    
    describe "derive5From6AndTopRight" $ do
        it "works"
            $ shouldBe
                (derive5From6AndTopRight "bcdefg" "a" uniquePatterns)
                "bcdef"

    describe "identifyBLSegment" $ do
        it "works"
            $ shouldBe
                (identifyBLSegment "bcdefg"  "bcdef")
                "g"

    describe "identifyBSegment" $ do
        it "works"
            $ shouldBe
                (identifyBSegment (get7 uniquePatterns) (get4 uniquePatterns) (get8 uniquePatterns) "g")
                "c"

    describe "derive9From8AndBottomLeft" $ do
        it "works"
            $ shouldBe
                (derive9From8AndBottomLeft (get8 uniquePatterns) "g")
                "abcdef"

    describe "derive3" $ do
        it "works"
            $ shouldBe
                (derive3 uniquePatterns)
                "abcdf"

    describe "derive0And2" $ do
        it "works"
            $ shouldBe
                (let one = get1 uniquePatterns
                     eight = get8 uniquePatterns
                     six = derive6From8And1 eight one uniquePatterns
                     topRightSegment = fst $ identifyTRAndBRSegments eight six one
                     five = derive5From6AndTopRight six topRightSegment uniquePatterns
                     three = derive3 uniquePatterns
                    in derive0And2 five three topRightSegment uniquePatterns
                )
                ("abcdeg", "acdfg")

    --describe "derive0" $ do
    --    it "works"
    --        $ shouldBe
    --            (derive0 "bcdefg" 'a' uniquePatterns)
    --            "5 representation is bcdef"

    describe "deduceDigitRepresentations" $ do
        it "works"
            $ shouldBe
                (sort $ deduceDigitRepresentations
                    (words $ normalize "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab")
                )

                (sort [
                    (sort "acedgfb", '8'),
                    (sort "cdfbe", '5'),
                    (sort "gcdfa", '2'),
                    (sort "fbcad", '3'),
                    (sort "dab", '7'),
                    (sort "cefabd", '9'),
                    (sort "cdfgeb", '6'),
                    (sort "eafb", '4'),
                    (sort "cagedb", '0'),
                    (sort "ab", '1')
                ])

    describe "p2ParseInput" $ do
        it "works"
            $ shouldBe
                (p2ParseInput "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")
                [(
                    normalize "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab",
                    normalize "cdfeb fcadb cdfeb cdbaf"
                )]

    describe "decodeFourDigitOutput" $ do
        it "works"
            $ shouldBe
                (decodeFourDigitOutput
                    $ head
                    $ p2ParseInput
                        "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
                )
                5353

        it "works for testInput"
            $ shouldBe
                (sum
                    (decodeFourDigitOutput
                        <$> p2ParseInput testInput)
                )
                61229
