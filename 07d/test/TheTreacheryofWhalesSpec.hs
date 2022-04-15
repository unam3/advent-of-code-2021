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
