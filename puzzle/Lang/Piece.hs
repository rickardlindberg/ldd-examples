module Lang.Piece where

import qualified Data.Set as S
import Lang.Pos

data Piece = Piece
    { symbol    :: Char
    , positions :: S.Set Pos
    } deriving (Ord, Eq)

pieceWidth :: Piece -> Int
pieceWidth (Piece _ positions) = 1 + maximum (map x (S.toList positions))

pieceHeight :: Piece -> Int
pieceHeight (Piece _ positions) = 1 + maximum (map y (S.toList positions))

posInPiece :: Pos -> Piece -> Bool
posInPiece pos (Piece _ positions) = pos `S.member` positions

movePiece :: Pos -> Piece -> Piece
movePiece pos (Piece symbol positions) = Piece symbol (S.map (addPos pos) positions)

getCombinations :: Piece -> [Piece]
getCombinations piece =
    let p1 = piece
        p2 = rotatePiece45 p1
        p3 = rotatePiece45 p2
        p4 = rotatePiece45 p3
        p5 = flipPiece p1
        p6 = flipPiece p2
        p7 = flipPiece p3
        p8 = flipPiece p4
    in  S.toList $ S.fromList [p1, p2, p3, p4, p5, p6, p7, p8]

flipPiece :: Piece -> Piece
flipPiece (Piece symbol locs) = normPiece $ Piece symbol (S.map posFlip locs)

rotatePiece45 :: Piece -> Piece
rotatePiece45 (Piece symbol locs) = normPiece $ Piece symbol (S.map posRotate45 locs)

normPiece :: Piece -> Piece
normPiece (Piece symbol locs) = Piece symbol newLocs
    where
        newLocs = S.map norm locs
        norm (Pos x y) = Pos (x-minX) (y-minY)
        minX = minimum $ map x $ S.toList locs
        minY = minimum $ map y $ S.toList locs

