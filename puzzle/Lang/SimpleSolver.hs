module Lang.SimpleSolver where

import Lang.Piece
import Lang.PuzzleState
import Lang.TreeSearch
import Lang.SolverCommon

findSolutions :: [Piece] -> Piece -> [PuzzleState]

stopLooking :: PuzzleNode -> Bool
stopLooking node = False

findSolutions pieces fitPiece = map puzzleState solutions
  where
    solutions   = search isSolution stopLooking generateChildren initialNode
    initialNode = newPuzzleNode pieces (emptyPuzzle fitPiece)

