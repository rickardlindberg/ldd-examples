module PuzzleState where

import Piece
import Pos
import Data.Maybe
import qualified Data.Set as S

data PuzzleState = PuzzleState
    { fitPiece :: Piece
    , pieces   :: [Piece]
    }

emptyPuzzle :: Piece -> PuzzleState
emptyPuzzle fitPiece = PuzzleState fitPiece []

putPiece :: Piece -> Pos -> PuzzleState -> Maybe PuzzleState
putPiece piece pos (PuzzleState fitPiece pieces) =
    if isOnFreeSpot && isInside
        then Just $ PuzzleState fitPiece (movedPiece:pieces)
        else Nothing
    where
        isInside       = movedPositions == (S.intersection movedPositions (positions fitPiece))
        isOnFreeSpot   = S.null (S.intersection movedPositions takenPositions)
        takenPositions = foldl S.union S.empty (map positions pieces)
        movedPositions = positions movedPiece
        movedPiece     = movePiece pos piece

printPuzzleState :: PuzzleState -> String

printPuzzleState state@(PuzzleState fitPiece _) =
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
pieceAt pos (PuzzleState _ pieces) = findPieceWithPos pieces pos
    where
        findPieceWithPos []     _ = Nothing
        findPieceWithPos (x:xs) p
            | posInPiece p x = Just x
            | otherwise      = findPieceWithPos xs p

