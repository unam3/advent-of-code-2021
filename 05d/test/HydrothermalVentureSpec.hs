module HydrothermalVentureSpec where 

import Data.Map.Strict (empty, fromList)
import Test.Hspec (Spec, describe, it, shouldBe)

import HydrothermalVenture

spec :: Spec
spec = do
    describe "parseInput" $ do
        it "works"
            $ shouldBe
                (parseInput "0,9 -> 5,9\n8,0 -> 0,8")
                [((0,9),(5,9)),((8,0),(0,8))]

    describe "getHVLinePoints" $ do
        it "works"
            $ shouldBe
                (getHVLinePoints ((1, 3), (4,3)))
                [(1,3),(2,3),(3,3),(4,3)]

    describe "markLinePoints" $ do
        it "works"
            $ shouldBe
                (markLinePoints empty ((1, 3), (4,3)))
                (fromList [((1,3), 1), ((2,3), 1), ((3,3), 1), ((4,3), 1)])
