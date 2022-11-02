why it compiles at all?

        it "WRONG TEST: works for testInput"
            $ shouldBe
                -- [Maybe String] -> [Maybe String]
                (fmap
                    (\maybeErrorString -> if isLineIncomplete maybeErrorString
                        then Nothing
                        else maybeErrorString
                    )
                    -- [String] -> [Maybe String]
                    $ fmap isLineCorruptedAndWhere
                    -- String -> [String]
                    $ parseInput testInput
                )
                $ pure [
                    Nothing,
                    Nothing,
                    Just "Expected open bracket for '}', but found '[' instead.",
                    Nothing,
                    Just "Expected open bracket for ')', but found '[' instead.",
                    Just "Expected open bracket for ']', but found '(' instead.",
                    Nothing,
                    Just "Expected open bracket for ')', but found '<' instead.",
                    Just "Expected open bracket for '>', but found '[' instead.",
                    Nothing
                ]

                (Just "Expected open bracket for '}', but found '[' instead.")
        
        it "works for testInput"
            $ shouldBe
                -- [Maybe String] -> [Maybe String]
                (fmap
                    (\maybeErrorString -> if isLineIncomplete maybeErrorString
                        then Nothing
                        else maybeErrorString
                    )
                    -- [String] -> [Maybe String]
                    $ fmap isLineCorruptedAndWhere
                    -- String -> [String]
                    $ parseInput testInput
                )
                (
                    [
                        Nothing,
                        Nothing,
                        Just "Expected open bracket for '}', but found '[' instead.",
                        Nothing,
                        Just "Expected open bracket for ')', but found '[' instead.",
                        Just "Expected open bracket for ']', but found '(' instead.",
                        Nothing,
                        Just "Expected open bracket for ')', but found '<' instead.",
                        Just "Expected open bracket for '>', but found '[' instead.",
                        Nothing
                    ]
                )

