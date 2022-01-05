module LanternfishSpec where 

import Data.Char (digitToInt)
import Data.List (group, sort)
import Data.Map.Strict (fromList)
import Test.Hspec (Spec, describe, it, shouldBe)

import Lanternfish

parsePuzzleTestOutputString :: String -> [(Int, Int)]
parsePuzzleTestOutputString = fmap (\ n -> (digitToInt $ head n, length n)) . tail . group . sort

spec :: Spec
spec = do
    describe "parseInput" $ do
        it "works"
            $ shouldBe
                (parseInput "3,4,3,1,2")
                (fromList [(3,2),(4,1),(1,1),(2,1)])

    describe "simulateDayCycle" $ do
        --it "works"
        --    $ shouldBe
        --        (simulateDayCycle $ parseInput "3,4,3,1,2")
        --        (fromList [(2,2),(3,1),(1,1),(0,1)])

        it "simulateNDayCycles 6: narrowed down"
            $ shouldBe
                (simulateDayCycle $ fromList [(6,2),(7,2),(8,1)])
                --(simulateNDayCycles 1 $ fromList [(6,2),(7,2),(8,1)])
                (fromList $ parsePuzzleTestOutputString "5,5,6,6,7")
          
    describe "simulateNDayCycle" $ do
        --it "simulateNDayCycles 2"
        --    $ shouldBe
        --        (simulateNDayCycles 2 (parseInput "3,4,3,1,2"))
        --        (fromList $ parsePuzzleTestOutputString "1,2,1,6,0,8")
          
        --it "simulateNDayCycles 3"
        --    $ shouldBe
        --        (simulateNDayCycles 3 (parseInput "3,4,3,1,2"))
        --        (fromList $ parsePuzzleTestOutputString "0,1,0,5,6,7,8")

        --it "simulateNDayCycles 4"
        --    $ shouldBe
        --        (simulateNDayCycles 4 (parseInput "3,4,3,1,2"))
        --        (fromList $ parsePuzzleTestOutputString "6,0,6,4,5,6,7,8,8")

        --it "simulateNDayCycles 4 #2"
        --    $ shouldBe
        --        (simulateNDayCycles 1 (parseInput "0,1,0,5,6,7,8"))
        --        (fromList $ parsePuzzleTestOutputString "6,0,6,4,5,6,7,8,8")
          
        --it "simulateNDayCycles 4: narrowed down issue"
        --    $ shouldBe
        --        (simulateNDayCycles 1 (parseInput "0,0,6,7"))
        --        (fromList $ parsePuzzleTestOutputString "6,6,5,6,8,8")

        --it "simulateNDayCycles 5"
        --    $ shouldBe
        --        (simulateNDayCycles 5 (parseInput "3,4,3,1,2"))
        --        -- [(3,1),(4,1),(5,3),(6,2),(7,2),(8,1)]
        --        (fromList $ parsePuzzleTestOutputString "5,6,5,3,4,5,6,7,7,8")

    --  it "simulateNDayCycles 6: narrowed down"
    --        $ shouldBe
    --            (simulateNDayCycles 1 $ fromList [(6,2),(7,2),(8,1)])
    --            (fromList $ parsePuzzleTestOutputString "5,5,6,6,7")

        --it "simulateNDayCycles 6"
        --    $ shouldBe
        --        (simulateNDayCycles 6 (parseInput "3,4,3,1,2"))
        --        (fromList $ parsePuzzleTestOutputString "4,5,4,2,3,4,5,6,6,7")

        --it "simulateNDayCycles 7"
        --    $ shouldBe
        --        (simulateNDayCycles 7 (parseInput "3,4,3,1,2"))
        --        (fromList $ parsePuzzleTestOutputString "3,4,3,1,2,3,4,5,5,6")

        --it "simulateNDayCycles 9"
        --    $ shouldBe
        --        (simulateNDayCycles 9 (parseInput "3,4,3,1,2"))
        --        (fromList $ parsePuzzleTestOutputString "1,2,1,6,0,1,2,3,3,4,8")

        it "simulateNDayCycles 18"
            $ shouldBe
                (simulateNDayCycles 18 (parseInput "3,4,3,1,2"))
                (fromList [(6, 5),(0,3),(4,2),(5,1),(1,5),(2,3),(3,2),(7,1),(8,4)])
