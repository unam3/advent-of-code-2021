module BinaryDiagnosticSpec where 

import Test.Hspec (Spec, describe, it, shouldBe)

import BinaryDiagnostic

spec :: Spec
spec = do
    describe "f" $ do
        it "works"
            $ shouldBe
                42
                42
