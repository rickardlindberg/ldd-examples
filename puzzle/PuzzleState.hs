module PuzzleState where

import Piece
import Pos
import Data.Maybe
import Data.List
import qualified Data.Set as S

data PuzzleState = PuzzleState
    { fitPiece :: Piece
    , pieces   :: [Piece]
    , freePos  :: S.Set Pos
    }

emptyPuzzle :: Piece -> PuzzleState
emptyPuzzle fitPiece = PuzzleState fitPiece [] (positions fitPiece)

putPiece :: Piece -> PuzzleState -> Maybe PuzzleState
putPiece piece (PuzzleState fitPiece pieces freePos) =
    if isOnFreeSpot
        then Just (PuzzleState fitPiece (piece:pieces) newFreePos)
        else Nothing
    where
        isOnFreeSpot = positions piece `S.isSubsetOf` freePos
        newFreePos   = S.difference freePos (positions piece)

printPuzzleState :: PuzzleState -> String

printPuzzleState state@(PuzzleState fitPiece _ _) =
    concatMap (\y -> (concatMap (\x -> posToString (Pos x y)) colNumbers) ++ "\n") lineNumbers
    where
        posToString pos
            | not (posInPiece pos fitPiece) = "  "
            | isNothing (pieceAt pos state) = symbol fitPiece:" "
            | otherwise                     = pieceToString (pieceAt pos state)
        pieceToString Nothing            = "  "
        pieceToString (Just (Piece c _)) = c:" "
        width                            = pieceWidth fitPiece
        height                           = pieceHeight fitPiece
        lineNumbers                      = [0..height-1]
        colNumbers                       = [0..width-1]

pieceAt :: Pos -> PuzzleState -> Maybe Piece
pieceAt pos (PuzzleState _ pieces _) = findPieceWithPos pieces pos
    where
        findPieceWithPos []     _ = Nothing
        findPieceWithPos (x:xs) p
            | posInPiece p x = Just x
            | otherwise      = findPieceWithPos xs p

printSolutions :: [PuzzleState] -> String
printSolutions states =
    let sol = states
    in  intercalate "\n" (map printPuzzleState sol) ++ show (length sol)

islands :: PuzzleState -> [S.Set Pos]
islands state = islandsFromPos (freePos state)

islandsFromPos :: S.Set Pos -> [S.Set Pos]
islandsFromPos positions
    | S.null positions = []
    | otherwise        = let aPos       = S.findMin positions
                             thisIsland = extractGroup S.empty (S.singleton aPos) positions
                         in  thisIsland : islandsFromPos (S.difference positions thisIsland)

extractGroup :: S.Set Pos -> S.Set Pos -> S.Set Pos -> S.Set Pos
extractGroup included toCheck free =
    if S.null toCheck || S.null free
        then included
        else extractGroup newIncluded newToCheck newFree
    where
        checkOk     = S.intersection toCheck free
        newIncluded = S.union included checkOk
        newToCheck  = S.fromList (concatMap posNeighbours (S.toList checkOk))
        newFree     = S.difference free checkOk

