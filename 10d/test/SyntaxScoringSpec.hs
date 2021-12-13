module SyntaxScoringSpec where 

import Test.Hspec (Spec, describe, it, shouldBe)

import SyntaxScoring

spec :: Spec
spec = do
    describe "isLineLegal" $ do
        it "returns True if closing bracket in the end corressond to opening bracket on start"
            $ shouldBe
                (isLineLegal "<{)")
                False

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

    describe "areChunksLegal" $ do

        it "works on empty chunk"
            $ shouldBe
                (areChunksLegal "()") 
                True

        it "works on nested chunks"
            $ shouldBe
                (areChunksLegal "({})") 
                True

        it "works on nested chunks #2"
            $ shouldBe
                (areChunksLegal "({<>})") 
                True

        it "works on nested series of chunks"
            $ shouldBe
                (areChunksLegal "(<{}[][]>)") 
                True

        it "works on same brackets inside the line"
            $ shouldBe
                (areChunksLegal "<<>>") 
                True
