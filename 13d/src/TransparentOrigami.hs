module TransparentOrigami where


import Data.List (isPrefixOf, partition, stripPrefix)
import Data.Vector (Vector, fromList)

type TransparentPaper = Vector (Int, Int)

replaceCommaWithWhitespace :: Char -> Char
replaceCommaWithWhitespace ',' = ' '
replaceCommaWithWhitespace anyChar = anyChar

makeTuple :: [Int] -> (Int, Int)
makeTuple [x, y] = (x, y)
makeTuple input = error $ "unsupported input: " ++ show input

type YLine = Int
type XLine = Int
data FoldInstruction = FoldUp YLine | FoldLeft XLine
    deriving (Eq, Show)

parseInstruction :: String -> FoldInstruction
parseInstruction string = case stripPrefix "fold along " string of
    Just ('x' : '=' : lineNumber) -> FoldLeft $ read lineNumber
    Just ('y' : '=' : lineNumber) -> FoldUp $ read lineNumber
    pluh -> error $ "stripping prefix gone wrong: " ++ show pluh

parseInput :: String -> (TransparentPaper, [FoldInstruction])
parseInput input =
    let (almostFoldInstructions, almostDotCoordinates) = partition (isPrefixOf "fold") $ lines input
        dotCoordinates = init almostDotCoordinates
        dotCoordinatesList = fmap
            (makeTuple
                . fmap (read :: String -> Int)
                . words
                . fmap replaceCommaWithWhitespace
            )
            dotCoordinates
        transparentPaper = fromList dotCoordinatesList
        foldInstructions = fmap parseInstruction almostFoldInstructions
    in (transparentPaper, foldInstructions)
