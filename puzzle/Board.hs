module Board where

import Piece
import Pos
import qualified Data.Set as S

data Board = Board
    { piecesLeft :: [Piece]
    , fitPiece   :: Piece
    , usedPieces :: [Piece]
    }

printBoard :: Board -> String

printBoard pieces = concatMap (\y -> (concatMap (\x -> posToString x y) colNumbers) ++ "\n") lineNumbers
    where
        posToString x y                  = shapeToString (pieceAt x y pieces)
        shapeToString Nothing            = "  "
        shapeToString (Just (Piece c _)) = c:" "
        width                            = 5
        height                           = 5
        lineNumbers                      = [0..height-1]
        colNumbers                       = [0..width-1]

pieceAt :: Int -> Int -> Board -> Maybe Piece
pieceAt x y (Board _ _ usedPieces) = findPieceWithPos usedPieces (Pos x y)
    where
        findPieceWithPos []     _ = Nothing
        findPieceWithPos (x:xs) p
            | p `S.member` (positions x) = Just x
            | otherwise                  = findPieceWithPos xs p

