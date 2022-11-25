module TransparentOrigami where


import Data.List (isPrefixOf, foldl', partition, stripPrefix)
import Data.Vector (Vector, cons, elem, empty, fromList, maximumBy)
import qualified Data.Vector as Vector
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

transposeBottomHalf :: Int -> TransparentPaper -> (Int, Int) -> TransparentPaper
transposeBottomHalf topY paperLowerHalf (x, y) =
    let bottomY = snd $ maximumBy (\a b -> compare (snd a) (snd b)) paperLowerHalf
    in undefined

foldPaper :: TransparentPaper -> FoldInstruction -> TransparentPaper
foldPaper paper (FoldUp yLineNumber) =
    let (paperUpperHalf, almostLowerPaperHalf) = Vector.partition ((< yLineNumber) . snd) paper
        -- remove folding line from the paper half
        -- "dots will never appear exactly on a fold line."
        paperLowerHalf = Vector.filter ((/= yLineNumber) . snd) almostLowerPaperHalf
        lowerPaperHalfTopY = yLineNumber + 1
        transposedLowerPaperHalf = Vector.foldl' (transposeBottomHalf lowerPaperHalfTopY) empty paperLowerHalf

        upperPaperHalfBottomY = yLineNumber - 1
        
        foldedPaper = Vector.foldl'
            (\ acc pointCoords ->
                if elem pointCoords acc
                then acc
                else cons pointCoords acc
            )
            paperUpperHalf
            paperLowerHalf
    in undefined

foldPaper paper (FoldLeft xLineNumber) = undefined
