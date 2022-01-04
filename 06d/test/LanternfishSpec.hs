module LanternfishSpec where 

import Test.Hspec (Spec, describe, it, shouldBe)

import Lanternfish

spec :: Spec
spec = do
    describe "parseInput" $ do
        it "works"
            $ shouldBe
                (parseInput "3,4,3,1,2")
                [3,4,3,1,2]

    describe "simulateNDayCycles" $ do
        it "works"
            $ shouldBe
                (simulateNDayCycles 18 (parseInput "3,4,3,1,2"))
                [6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8]
