module TheTreacheryofWhalesSpec where 

import qualified Data.IntMap.Strict as IntMap
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import TheTreacheryofWhales


spec :: Spec
spec = do
    parsedTestInput <- runIO $ readFile "testInput"

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

    describe "alignFoldF" $ do
        it "works with equal align and horizontal positions"
            $ shouldBe
                (alignFoldF 10 0 10 (20, Nothing))
                0
        it "works with align position bigger than horizontal position"
            $ shouldBe
                (alignFoldF 20 0 10 (20, Nothing))
                200
        it "works with align position lesser than horizontal position"
            $ shouldBe
                (alignFoldF 10 0 20 (20, Nothing))
                200
