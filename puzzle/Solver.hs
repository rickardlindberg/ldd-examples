module Solver where

import Piece
import PuzzleState
import TreeSearch

findSolutions :: [Piece] -> Piece -> [PuzzleState]

data PuzzleNode = PuzzleNode
    { piecesLeft  :: [Piece]
    , puzzleState :: PuzzleState
    }

instance TreeNode PuzzleNode where
    generateSubTree node = []
    isBad           node = True

findSolutions pieces fitPiece = map puzzleState (search (PuzzleNode pieces (emptyPuzzle fitPiece)))

