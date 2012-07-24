module Piece where

import qualified Data.Set as S
import Pos

data Piece = Piece
    { symbol    :: Char
    , positions :: S.Set Pos
    }

movePiece :: Pos -> Piece -> Piece
movePiece pos (Piece symbol positions) = Piece symbol (S.map (addPos pos) positions)

pieceWidth :: Piece -> Int
pieceWidth (Piece _ positions) = 1 + maximum (map x (S.toList positions))

pieceHeight :: Piece -> Int
pieceHeight (Piece _ positions) = 1 + maximum (map y (S.toList positions))

posInPiece :: Pos -> Piece -> Bool
posInPiece pos (Piece _ positions) = pos `S.member` positions

