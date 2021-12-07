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

    describe "makeBoardWithStateMap" $ do
        it "returns BoardWithStateMap"
            $ shouldBe
                (makeBoardWithStateMap [
                          ([[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]], mempty)
                        , ([[22,13,17,11,0],[8,2,23,4,24],[21,9,14,16,7],[6,10,3,18,5],[1,12,20,15,19]], mempty)
                    ]
                )
                (fromList [
                      (0,([[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]], mempty))
                    , (1,([[22,13,17,11,0],[8,2,23,4,24],[21,9,14,16,7],[6,10,3,18,5],[1,12,20,15,19]], mempty))
                ])

    describe "getSublistAfterElement" $ do
        it "works"
            $ shouldBe
                (getSublistAfterElement 
                    4
                    [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
                )
                [9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]

    describe "removeBoard" $ do
        it "works"
            $ shouldBe
                (removeBoard
                    (fromList [
                          (0,([[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]], mempty))
                        , (1,([[22,13,17,11,0],[8,2,23,4,24],[21,9,14,16,7],[6,10,3,18,5],[1,12,20,15,19]], mempty))
                    ])
                    [[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]]

                )
                (fromList [
                    (1,([[22,13,17,11,0],[8,2,23,4,24],[21,9,14,16,7],[6,10,3,18,5],[1,12,20,15,19]], mempty))
                ])

    --describe "drawNumberUntilPossibleWin" $ do
    --    it "returns winning drawn number wrapped in Just"
    --        $ shouldBe
    --            (drawNumberUntilPossibleWin
    --                [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
    --                ([[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]], empty)
    --            )
    --            (Just 24)

    --    it "returns Nothing if board doesn't win"
    --        $ shouldBe
    --            (drawNumberUntilPossibleWin
    --                [7]
    --                ([[22,13,17,11,0],[8,2,23,4,24],[21,9,14,16,7],[6,10,3,18,5],[1,12,20,15,19]], empty)
    --            )
    --            Nothing
