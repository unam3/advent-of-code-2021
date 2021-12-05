module GiantSquidSpec where 

import Data.Map.Strict (fromList)
import Test.Hspec (Spec, describe, it, shouldBe)

import GiantSquid

spec :: Spec
spec = do
    describe "checkIfBoardWin" $ do
        it "return True if column is fully marked"
            -- replace with proper function to test True values
            $ shouldBe
                (checkIfBoardWin
                    (fromList [
                          ((0,3), True)
                        , ((1,3), True)
                        , ((2,3), True)
                        , ((3,3), True)
                        , ((4,3), True)
                    ])
                    (3,3)
                )
                True
        it "return True if row is fully marked"
            $ shouldBe
                (checkIfBoardWin
                    (fromList [
                          ((3,0), True)
                        , ((3,1), True)
                        , ((3,2), True)
                        , ((3,3), True)
                        , ((3,4), True)
                    ])
                    (3,3)
                )
                True
        it "return False if no column or row is fully marked"
            $ shouldBe
                (checkIfBoardWin
                    (fromList [
                          ((0,3), True)
                        , ((1,3), True)
                        , ((2,3), True)
                        , ((3,3), True)
                    ])
                    (3,3)
                )
                False
