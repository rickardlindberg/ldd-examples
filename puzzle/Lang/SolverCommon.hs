module Lang.SolverCommon where

import Lang.Piece
import Lang.Pos
import Lang.PuzzleState
import Data.Maybe

data PuzzleNode = PuzzleNode
    { piecesLeft  :: [[Piece]]
    , puzzleState :: PuzzleState
    }

newPuzzleNode :: [Piece] -> PuzzleState -> PuzzleNode
newPuzzleNode pieces state = PuzzleNode expandedPieces state
    where
        expandedPieces = map expandPiece pieces
        expandPiece piece =
            let comb = getCombinations piece
            in  concatMap (\piece -> possiblePositions piece state) comb

possiblePositions :: Piece -> PuzzleState -> [Piece]
possiblePositions piece state =
    map (\pos -> movePiece pos piece) [Pos x y | x <- [0..maxX], y <- [0..maxY]]
    where
        maxX = pieceWidth  (fitPiece state) - pieceWidth  piece
        maxY = pieceHeight (fitPiece state) - pieceHeight piece

isSolution :: PuzzleNode -> Bool
isSolution node = null (piecesLeft node)

generateChildren :: PuzzleNode -> [PuzzleNode]
generateChildren node = map (PuzzleNode restPieces) newStates
    where
        firstPiece = head (piecesLeft node)
        restPieces = tail (piecesLeft node)
        newStates  = mapMaybe (\piece -> putPiece piece (puzzleState node)) firstPiece

