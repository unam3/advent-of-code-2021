module TransparentOrigami where


import Data.List (isPrefixOf, foldl', partition, stripPrefix)
import Data.Vector (Vector, elem, fromList, maximumBy)
import Prelude hiding (elem)

type TransparentPaper = Vector (Int, Int)

sharpOrDot :: TransparentPaper -> (Int, Int) -> String
sharpOrDot paper (x, y) =
    if elem (x, y) paper
    then "#"
    else "."
    

plot :: (Int, Int) -> ((Int, Int) -> String) -> String
plot (maxX, maxY) f =
    let string' =
            foldl'
                (\ string y ->
                    string ++ (foldl'
                        (\ row x ->
                            --let toPlot = show (x, y)
                            let toPlot = f (x, y)
                            in row ++ toPlot
                        )
                        ""
                        [0..maxX]
                    ) ++ "\n"
                )
                ""
                [0..maxY]
    in string'

visualizeTransparentPaper :: TransparentPaper -> String
visualizeTransparentPaper paper =
    let mostLargestValuesOfDotsCoordinates =
            (
            fst $ maximumBy (\a b -> compare (fst a) (fst b)) paper,
            snd $ maximumBy (\a b -> compare (snd a) (snd b)) paper
            )
    in plot mostLargestValuesOfDotsCoordinates (sharpOrDot paper)

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
