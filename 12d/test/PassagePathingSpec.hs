module PassagePathingSpec where 

import Data.Map.Strict (fromList)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import PassagePathing

constructMore :: [Path] -> String -> [Path]
constructMore constructPathsResults input =
    concat
        $ fmap (\ path -> constructPaths path (parseInput input)) constructPathsResults

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
        it "exclude paths which does not reach \"end\": start-b-d"
            $ shouldBe
                (constructPathsWrapper
                    -- fromList [("a",["b"]),("b",["end","a"]),("start",["a"])]
                    $ parseInput "a-start\na-b"
                )
                []
        it "constructs test path"
            $ shouldBe
                (constructPathsWrapper
                    -- fromList [("a",["b"]),("b",["end","a"]),("start",["a"])]
                    $ parseInput "a-start\na-b\nend-b"
                )
                [["start", "a", "b","end"]]
        it "construct paths for first testInput"
            $ shouldBe
                (constructPathsWrapper $ parseInput testInput)
                [["start","A","c","A","b","A","end"],["start","A","c","A","b","end"],["start","A","c","A","end"],["start","A","b","A","c","A","end"],["start","A","b","A","end"],["start","A","b","end"],["start","A","end"],["start","b","A","c","A","end"],["start","b","A","end"],["start","b","end"]]
