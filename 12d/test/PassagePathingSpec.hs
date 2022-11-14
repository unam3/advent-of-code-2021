module PassagePathingSpec where 

import Data.Map.Strict (fromList)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import PassagePathing

constructMore :: [Path] -> String -> [Path]
constructMore constructPathsResults input =
    concatMap (\ path -> constructPaths path (parseInput input)) constructPathsResults

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"

    testInput2 <- runIO $ readFile "testInput2"

    testInput3 <- runIO $ readFile "testInput3"

    input <- runIO $ readFile "input.txt"

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
        it "construct paths for testInput2"
            $ shouldBe
                (constructPathsWrapper $ parseInput testInput2)
                [["start","HN","dc","end"],["start","HN","dc","HN","end"],["start","HN","dc","HN","kj","HN","end"],["start","HN","dc","kj","HN","end"],["start","HN","end"],["start","HN","kj","HN","dc","end"],["start","HN","kj","HN","dc","HN","end"],["start","HN","kj","HN","end"],["start","HN","kj","dc","end"],["start","HN","kj","dc","HN","end"],["start","kj","HN","dc","end"],["start","kj","HN","dc","HN","end"],["start","kj","HN","end"],["start","kj","dc","end"],["start","kj","dc","HN","end"],["start","dc","end"],["start","dc","HN","end"],["start","dc","HN","kj","HN","end"],["start","dc","kj","HN","end"]]
        it "construct right number of paths for testInput3"
            $ shouldBe
                (length $ constructPathsWrapper $ parseInput testInput3)
                226
        it "construct right number of paths for input"
            $ shouldBe
                (length $ constructPathsWrapper $ parseInput input)
                4167

    describe "getSmallCavesNames" $ do
        it "returns small cave names list"
            $ shouldBe
                (getSmallCavesNames $ parseInput "a-start\na-b\nend-b")
                ["a", "b"]

    describe "modifyRelationsToVisitSmallCaveTwice" $ do
        it ""
            $ shouldBe
                {-

                     start                start    
                       |                  /   \    
                       a       ->        a-----a1  
                      |                  \   /    
                      b                    b
                     |                     |
                    end                   end
                -}
                (modifyRelationsToVisitSmallCaveTwice (parseInput "a-start\na-b\nend-b") "a")
                $ fromList [("a",["b"]),("asecond",["b"]),("b",["end","a", "asecond"]),("start",["a", "asecond"])]
