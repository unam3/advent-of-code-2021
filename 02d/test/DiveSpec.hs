module DiveSpec where 

import Test.Hspec (Spec, describe, it, shouldBe)

import Dive

spec :: Spec
spec = do
    describe "f" $ do
        it "works"
            $ shouldBe
                42
                42
