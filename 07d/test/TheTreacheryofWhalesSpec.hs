module TheTreacheryofWhalesSpec where 

import qualified Data.IntMap.Strict as IntMap
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import TheTreacheryofWhales


spec :: Spec
spec = do
    parsedTestInput <- runIO $ readFile "testInput"

    parsedInput <- runIO $ readFile "input.txt"

    describe "parseInput" $ do
        it "works"
            $ shouldBe
                (parseInput parsedTestInput)
                (IntMap.fromList [
                    (16, (1, Nothing)),
                    (1, (2, Nothing)),
                    (2, (3, Nothing)),
                    (0, (1, Nothing)),
                    (4, (1, Nothing)),
                    (7, (1, Nothing)),
                    (14, (1, Nothing))
                ])

    describe "fuelCostCount" $ do
        it "works with equal align and horizontal positions"
            $ shouldBe
                (fuelCostCount 10 id 0 10 (20, Nothing))
                0
        it "works with align position bigger than horizontal position"
            $ shouldBe
                (fuelCostCount 20 id 0 10 (20, Nothing))
                200
        it "works with align position lesser than horizontal position"
            $ shouldBe
                (fuelCostCount 10 id 0 20 (20, Nothing))
                200

    describe "countTotalFuelToAlign" $ do
        it "works"
            $ shouldBe
                (countTotalFuelToAlign (parseInput parsedTestInput) 2 id)
                37

        it "works for part 2 and testInput"
            $ shouldBe
                (countTotalFuelToAlign (parseInput parsedTestInput) 2 countProperFuelCost)
                206

    --describe "countTotalFuelToAlign'" $ do
    --    it "works"
    --        $ shouldBe
    --            (countTotalFuelToAlign' (parseInput parsedTestInput) 2 countProperFuelCost)
    --            (206, "")


    describe "getLeastFuelCostAlignPosition" $ do
        it "works with test input"
            $ shouldBe
                (getResultsWithoutDebug . getLeastFuelCostAlignPosition id $ parseInput parsedTestInput)
                (2, 37)

        it "works with real input"
            $ shouldBe
                (getResultsWithoutDebug . getLeastFuelCostAlignPosition id $ parseInput parsedInput)
                (342,351901)

        it "works for second puzzle part and testInput"
            $ shouldBe
                (getResultsWithoutDebug
                    . getLeastFuelCostAlignPosition countProperFuelCost $ parseInput parsedTestInput)
                (5,168)

    describe "countProperFuelCost" $ do
        it "works for 11"
            $ shouldBe
                (countProperFuelCost 11)
                66
