module PuzzleState where

import Piece
import Pos
import Data.Maybe
import Data.List
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

printSolutions :: [PuzzleState] -> String
printSolutions states = intercalate "\n" (map printPuzzleState states)

islands :: PuzzleState -> [S.Set Pos]
islands state = islandsFromPos freePositions
    where
        freePositions     = S.difference fitPiecePositions placedPositions
        fitPiecePositions = positions $ fitPiece state
        placedPositions   = foldl S.union S.empty (map positions (pieces state))

islandsFromPos :: S.Set Pos -> [S.Set Pos]
islandsFromPos positions
    | S.null positions = []
    | otherwise        = let aPos       = S.findMin positions
                             thisIsland = extractGroup aPos positions
                         in  thisIsland : islandsFromPos (S.difference positions thisIsland)

extractGroup :: Pos -> S.Set Pos -> S.Set Pos
extractGroup pos positions
    | pos `S.notMember` positions = S.empty
    | otherwise                   = inner (S.singleton pos)
                                          (S.fromList (posNeighbours pos))
                                          (S.delete pos positions)
    where
        inner :: S.Set Pos -> S.Set Pos -> S.Set Pos -> S.Set Pos
        inner included toCheck free =
            if S.null toCheck || S.null free
                then included
                else inner newIncluded newToCheck newFree
            where
                checkOk     = S.intersection toCheck free
                newIncluded = S.union included checkOk
                newToCheck  = S.fromList (concatMap posNeighbours (S.toList checkOk))
                newFree     = S.difference free checkOk

