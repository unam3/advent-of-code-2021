module SonarSweepSpec where 

import Test.Hspec (Spec, describe, it, shouldBe)

import SonarSweep

spec :: Spec
spec = do
    describe "parseInput" $ do
        it "works"
            $ shouldBe
                (parseInput "21\n43\n0")
                ([21,43,0] :: [Int])

    describe "makeListOfTuples" $ do
        it "works"
            $ shouldBe
                (makeListOfTuples $ parseInput "21\n43\n0")
                [(21,43),(43,0)]
