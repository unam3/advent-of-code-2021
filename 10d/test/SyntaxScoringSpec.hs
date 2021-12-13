module SyntaxScoringSpec where 

import Test.Hspec (Spec, describe, it, shouldBe)

import SyntaxScoring

spec :: Spec
spec = do
    describe "isNumberOfCharsEven" $ do
        it "returns False if number of characters in line is odd"
            $ shouldBe
                (isNumberOfCharsEven "<{)")
                False

    describe "getLineChunks" $ do
        it "returns substring without leading and ending characters"
            $ shouldBe
                (getLineChunks "<{)")
                "{"

    describe "getCorruptedChunk" $ do

        it "works on empty chunk"
            $ shouldBe
                (getCorruptedChunk "()") 
                Proceed

        it "works on nested chunks"
            $ shouldBe
                (getCorruptedChunk "({})") 
                Proceed

        it "works on nested chunks #2"
            $ shouldBe
                (getCorruptedChunk "({<>})") 
                Proceed

        it "works on nested series of chunks"
            $ shouldBe
                (getCorruptedChunk "(<{}[][]>)") 
                Proceed

        it "works on nested series of chunks #2"
            $ shouldBe
                (getCorruptedChunk "(<{<>}[][()]>)") 
                Proceed

        it "works on same brackets inside the line"
            $ shouldBe
                (getCorruptedChunk "<<>>") 
                Proceed

        it "works with illegal lines from testInput"
            $ shouldBe
                (getCorruptedChunk "[({(<(())[]>[[{[]{<()<>>") 
                Proceed

        it "works with illegal lines #2 from testInput"
            $ shouldBe
                (getCorruptedChunk "[(()[<>])]({[<{<<[]>>(") 
                Proceed
