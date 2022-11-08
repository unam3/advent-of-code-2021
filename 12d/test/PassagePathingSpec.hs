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
        it ""
            $ shouldBe
                (parseInput "a-start\na-b\nend-b")
                $ fromList [("a",["b"]),("b",["end","a"]),("start",["a"])]

    describe "constructPaths(Wrapper)" $ do
        it "construct simplest paths"
            $ shouldBe
                (constructPathsWrapper  
                    -- fromList [("a",["b"]),("b",["end","a"]),("start",["a"])]
                    $ parseInput "a-start\na-b\nend-b"
                )
                [["start", "a", "b", "end"]]
        it "construct paths for first testInput"
            $ shouldBe
                (constructPathsWrapper  
                    $ parseInput testInput
                )
                [["start", "a", "b", "end"]]
