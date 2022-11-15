module TransparentOrigamiSpec where 

import Data.Vector (empty)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import TransparentOrigami

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"

    --input <- runIO $ readFile "input.txt"

    describe "parseInput" $ do
        it "works"
            $ shouldBe
                (parseInput testInput)
                (empty, [])

    describe "f" $ do
        it "works"
            $ shouldBe
                42
                42
