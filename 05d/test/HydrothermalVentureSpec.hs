module HydrothermalVentureSpec where 

import Data.List (sort)
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

    describe "markHVLinePoints" $ do
        it "works"
            $ shouldBe
                (markHVLinePoints empty ((1, 3), (4,3)))
                (fromList [((1,3), 1), ((2,3), 1), ((3,3), 1), ((4,3), 1)])

    describe "getDiagonalLinePoints" $ do
        it "works on ((1, 1), (3, 3))"
            $ shouldBe
                (sort $ getDiagonalLinePoints ((1, 1), (3, 3)))
                [(1,1),(2,2),(3,3)]

        it "works on ((3, 3), (1, 1))"
            $ shouldBe
                (sort $ getDiagonalLinePoints ((3, 3), (1, 1)))
                [(1,1),(2,2),(3,3)]

        it "works on ((9, 7), (7, 9))"
            $ shouldBe
                (sort $ getDiagonalLinePoints ((9, 7), (7, 9)))
                [(7,9),(8,8),(9,7)]

        it "works on ((7, 9), (9, 7))"
            $ shouldBe
                (sort $ getDiagonalLinePoints ((7, 9), (9, 7)))
                [(7,9),(8,8),(9,7)]

