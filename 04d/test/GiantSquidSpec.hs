module GiantSquidSpec where 

import Test.Hspec (Spec, describe, it, shouldBe)

import GiantSquid

spec :: Spec
spec = do
    describe "f" $ do
        it "works"
            $ shouldBe
                42
                42
