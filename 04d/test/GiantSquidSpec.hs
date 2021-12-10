module GiantSquidSpec where 

import Data.Map.Strict ((!), fromList)
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
                    0
                )
                (fromList [
                    (1,([[22,13,17,11,0],[8,2,23,4,24],[21,9,14,16,7],[6,10,3,18,5],[1,12,20,15,19]], mempty))
                ])

    --describe "getNextAvailableBoardWithStateIndex" $ do
    --    it "works"
    --        $ shouldBe
    --            (getNextAvailableBoardWithStateIndex
    --                (fromList [
    --                      (0,([[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]], mempty))
    --                    , (1,([[22,13,17,11,0],[8,2,23,4,24],[21,9,14,16,7],[6,10,3,18,5],[1,12,20,15,19]], mempty))
    --                ])
    --                0
    --            )
    --            (Just 1)
    
    describe "getNonMarkedBoardWithStateIndexes" $ do
        it "returns [1] if 0 assumed marked in [0,1]"
            $ shouldBe
                (getNonMarkedBoardWithStateIndexes
                    (fromList [
                          (0,([[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]], mempty))
                        , (1,([[22,13,17,11,0],[8,2,23,4,24],[21,9,14,16,7],[6,10,3,18,5],[1,12,20,15,19]], mempty))
                    ])
                    0
                )
                [1]

        it "returns [] if 1 assumed marked in [0,1]"
            $ shouldBe
                (getNonMarkedBoardWithStateIndexes
                    (fromList [
                          (0,([[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]], mempty))
                        , (1,([[22,13,17,11,0],[8,2,23,4,24],[21,9,14,16,7],[6,10,3,18,5],[1,12,20,15,19]], mempty))
                    ])
                    1
                )
                []

        it "returns [] if 1 assumed marked in [1]"
            $ shouldBe
                (getNonMarkedBoardWithStateIndexes
                    (fromList [
                          (1,([[22,13,17,11,0],[8,2,23,4,24],[21,9,14,16,7],[6,10,3,18,5],[1,12,20,15,19]], mempty))
                    ])
                    1
                )
                []

--    describe "drawNumberUntilFirstPossibleWin" $ do
--        it "works in one-element boardWithStateMap"
--            $ shouldBe
--                (drawNumberUntilFirstPossibleWin
--                    [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
--                    (
--                        fromList [(
--                            0,
--                            ([[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]], mempty)
--                        )]
--                    )
--                    [0]
--                )
--                (Just (
--                    0,
--                    24,
--                    fromList [(0,([[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]],fromList [((0,0),True),((0,1),True),((0,2),True),((0,3),True),((0,4),True),((1,3),True),((2,2),True),((3,1),True),((3,4),True),((4,0),True),((4,1),True),((4,4),True)]))]
--                ))
--
--        it "works in one-element non-zero index of boardWithStateMap element"
--            $ shouldBe
--                (drawNumberUntilFirstPossibleWin
--                    [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
--                    (
--                        fromList [(
--                            5,
--                            ([[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]], mempty)
--                        )]
--                    )
--                    [5]
--                )
--                (Just (
--                    5,
--                    24,
--                    fromList [(5,([[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]],fromList [((0,0),True),((0,1),True),((0,2),True),((0,3),True),((0,4),True),((1,3),True),((2,2),True),((3,1),True),((3,4),True),((4,0),True),((4,1),True),((4,4),True)]))]
--                ))
--
--        it "return Nothing if called with one-element non-zero wrong index of boardWithStateMap element"
--            $ shouldBe
--                (drawNumberUntilFirstPossibleWin
--                    [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
--                    (
--                        fromList [(
--                            5,
--                            ([[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]], mempty)
--                        )]
--                    )
--                    [42]
--                )
--                Nothing

    describe "drawNumberUntilLastPossibleWin'" $ do

        it "go through all the boards (winning order: 1, 0)"
            $ shouldBe
                (drawNumberUntilLastPossibleWin'
                    (
                        [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1],
                        fromList [
                            (
                                0,
                                ([[3,15,0,2,22],[9,18,13,17,5],[19,8,7,25,23],[20,11,10,24,4],[14,21,16,12,6]], mempty)
                            ),
                            (
                                1,
                                ([[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]], mempty)
                            )
                        ],
                        0
                    )
                    Nothing
                )
                (Just (0, 13, fromList [(0,([[3,15,0,2,22],[9,18,13,17,5],[19,8,7,25,23],[20,11,10,24,4],[14,21,16,12,6]],fromList [((0,2),True),((0,3),True),((1,0),True),((1,2),True),((1,3),True),((1,4),True),((2,2),True),((2,4),True),((3,1),True),((3,2),True),((3,3),True),((3,4),True),((4,0),True),((4,1),True),((4,2),True)]))]))
        
        it "go through all the boards (winning order: 0, 1)"
            $ shouldBe
                (drawNumberUntilLastPossibleWin'
                    (
                        [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1],
                        fromList [
                            (
                                0,
                                ([[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]], mempty)
                            ),
                            (
                                1,
                                ([[3,15,0,2,22],[9,18,13,17,5],[19,8,7,25,23],[20,11,10,24,4],[14,21,16,12,6]], mempty)
                            )
                        ],
                        0
                    )
                    Nothing
                )
                (Just (1, 13, fromList [(1,([[3,15,0,2,22],[9,18,13,17,5],[19,8,7,25,23],[20,11,10,24,4],[14,21,16,12,6]],fromList [((0,2),True),((0,3),True),((1,0),True),((1,2),True),((1,3),True),((1,4),True),((2,2),True),((2,4),True),((3,1),True),((3,2),True),((3,3),True),((3,4),True),((4,0),True),((4,1),True),((4,2),True)]))]))

        it "go through all the boards (winning order: 1, 4)"
            $ shouldBe
                (drawNumberUntilLastPossibleWin'
                    (
                        [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1],
                        fromList [
                            (
                                1,
                                ([[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]], mempty)
                            ),
                            (
                                4,
                                ([[3,15,0,2,22],[9,18,13,17,5],[19,8,7,25,23],[20,11,10,24,4],[14,21,16,12,6]], mempty)
                            )
                        ],
                        0
                    )
                    Nothing
                )
                (Just (4, 13, fromList [(4,([[3,15,0,2,22],[9,18,13,17,5],[19,8,7,25,23],[20,11,10,24,4],[14,21,16,12,6]],fromList [((0,2),True),((0,3),True),((1,0),True),((1,2),True),((1,3),True),((1,4),True),((2,2),True),((2,4),True),((3,1),True),((3,2),True),((3,3),True),((3,4),True),((4,0),True),((4,1),True),((4,2),True)]))]))

        it "go through all the boards (winning order: 4, 1)"
            $ shouldBe
                (drawNumberUntilLastPossibleWin'
                    (
                        [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1],
                        fromList [
                            (
                                1,
                                ([[3,15,0,2,22],[9,18,13,17,5],[19,8,7,25,23],[20,11,10,24,4],[14,21,16,12,6]], mempty)
                            ),
                            (
                                4,
                                ([[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]], mempty)
                            )
                        ],
                        0
                    )
                    Nothing
                )
                (Just (1, 13, fromList [(1,([[3,15,0,2,22],[9,18,13,17,5],[19,8,7,25,23],[20,11,10,24,4],[14,21,16,12,6]],fromList [((0,2),True),((0,3),True),((1,0),True),((1,2),True),((1,3),True),((1,4),True),((2,2),True),((2,4),True),((3,1),True),((3,2),True),((3,3),True),((3,4),True),((4,0),True),((4,1),True),((4,2),True)]))]))

    describe "solution of the second puzzle part" $ do
        it "works"
            $ shouldBe
                ((\ maybeBoardWithStateAndDrawnNumber ->
                    case maybeBoardWithStateAndDrawnNumber of
                        Just (boardWithStateIndex, drawnNumber, boardWithStateMap) ->
                            calculateWinningScore (boardWithStateMap ! boardWithStateIndex) drawnNumber
                        Nothing -> error "no winning board"
                )
                $ drawNumberUntilLastPossibleWin'
                    (
                        [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1],
                        fromList [
                            (
                                0,
                                ([[22,13,17,11,0],[8,2,23,4,24],[21,9,14,16,7],[6,10,3,18,5],[1,12,20,15,19]], mempty)
                            ),
                            (
                                1,
                                ([[3,15,0,2,22],[9,18,13,17,5],[19,8,7,25,23],[20,11,10,24,4],[14,21,16,12,6]], mempty)
                            ),
                            (
                                2,
                                ([[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]], mempty)
                            )
                        ],
                        0
                    )
                    Nothing
                )
                1924
