module DumboOctopusSpec where 

import Data.Map.Strict (fromList)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import DumboOctopus

spec :: Spec
spec = do
    parseInputTestInput <- runIO $ readFile "parseInputTestInput"

    simulateStepInput <- runIO $ readFile "simulateStepInput"

    --testInput <- runIO $ readFile "testInput"

    --input <- runIO $ readFile "input.txt"

    describe "parseInput" $ do
        it "works"
            $ shouldBe
                (parseInput parseInputTestInput)
                $ fromList [
                    ((0, 0), 2),
                    ((1, 0), 4),
                    ((0, 1), 7),
                    ((1, 1), 5)
                ]

    describe "increaseAdjacentLevels" $ do
        it "works"
            $ shouldBe
                (increaseAdjacentLevels (parseInput parseInputTestInput) (1,1))
                $ fromList [
                    ((0, 0), 3),
                    ((1, 0), 5),
                    ((0, 1), 8),
                    ((1, 1), 5)
                ]

    describe "filterIncreaseLoop" $ do
        it "works for loops"
            $ shouldBe
                (filterIncreaseLoop (fmap (+1)
                    $ parseInput simulateStepInput) ([], [(2,2)]))
                (uncurry filterIncreaseLoop
                    $ uncurry filterIncreaseLoop
                    $ filterIncreaseLoop (fmap (+1)
                    $ parseInput simulateStepInput) ([], [(2,2)]))

    --describe "simulateStep" $ do
    --    it "works on test data"
    --        $ shouldBe
    --            (simulateStep (parseInput simulateStepInput, 0))
    --            (parseInput simulateStepOutputELShouldBe, 9)
