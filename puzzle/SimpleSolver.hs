module SimpleSolver where

import Piece
import PuzzleState
import TreeSearch
import SolverCommon

findSolutions :: [Piece] -> Piece -> [PuzzleState]

stopLooking :: PuzzleNode -> Bool
stopLooking node = False

findSolutions pieces fitPiece = map puzzleState solutions
    where
        solutions   = search isSolution stopLooking generateChildren initialNode
        initialNode = newPuzzleNode pieces (emptyPuzzle fitPiece)

