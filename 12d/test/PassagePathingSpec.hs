module PassagePathingSpec where 

import Data.Map.Strict (fromList)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import PassagePathing

constructMore :: (Path, [Path]) -> String -> [(Path, [Path])]
constructMore constructPathsResults input =
    fmap (\ path -> constructPaths path (parseInput input)) $ snd constructPathsResults

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
                ([], [])
        it "construct paths for first testInput"
            $ shouldBe
                (fmap (`constructMore` testInput)
                {- [
                    (
                        ["b","start"],
                        [["end","b","start"],["d","b","start"],["A","b","start"]]
                    ),
                    (
                        ["A","start"],
                        [["end","A","start"],["b","A","start"],["c","A","start"]]
                    )
                ] -}
                $ constructMore (constructPathsWrapper $ parseInput testInput) testInput)
                --(fmap (\ path -> constructPaths path (parseInput testInput)) $ snd
                --    -- (["start"],[["b","start"],["A","start"]])
                --    $ constructPathsWrapper
                --    $ parseInput testInput
                --)
                []
