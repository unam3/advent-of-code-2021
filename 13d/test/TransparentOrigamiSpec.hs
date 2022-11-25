module TransparentOrigamiSpec where 

import Data.Vector (fromList)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import TransparentOrigami

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"
    visualizeReference <- runIO $ readFile "visualizeReference"
    firstFoldVisualizedReference <- runIO $ readFile "firstFoldVisualizedReference"

    --input <- runIO $ readFile "input.txt"

    describe "parseInput" $ do
        it "works"
            $ shouldBe
                (parseInput testInput)
                (fromList [(6,10),(0,14),(9,10),(0,3),(10,4),(4,11),(6,0),(6,12),(4,1),(0,13),(10,12),(3,4),(3,0),(8,4),(1,10),(2,14),(8,10),(9,0)],[FoldUp 7,FoldLeft 5])

    describe "visualizeTransparentPaper" $ do
        it ""
            $ shouldBe
                (visualizeTransparentPaper $ fst $ parseInput testInput)
                visualizeReference

    describe "foldPaper" $ do
        it "works"
            $ shouldBe
                (visualizeTransparentPaper
                    $ (\ (transparentPaper, foldInstructions) -> foldPaper transparentPaper $ head foldInstructions
                    )
                    $ (parseInput testInput)
                )
                firstFoldVisualizedReference
