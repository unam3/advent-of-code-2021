module PassagePathingSpec where 

import Data.Map.Strict (fromList)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import PassagePathing

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"

    --testInput2 <- runIO $ readFile "testInput2"

    --testInput3 <- runIO $ readFile "testInput3"

    --input <- runIO $ readFile "input.txt"

    describe "parseInput" $ do
        it "works"
            $ shouldBe
                (parseInput testInput)
                $ fromList []

    describe "f" $ do
        it "works"
            $ shouldBe
                42
                42
