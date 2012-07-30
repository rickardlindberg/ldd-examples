module Lang.PruningSolver where

import Lang.Pos
import Lang.Piece
import Lang.PuzzleState
import Lang.TreeSearch
import Lang.SolverCommon
import qualified Data.Set as S

findSolutionsBetter :: [Piece] -> Piece -> [PuzzleNode -> Bool] -> [PuzzleState]
findSolutionsBetter pieces fitPiece rules = map puzzleState solutions
  where
    solutions = search isSolution stopLooking generateChildren initialNode
    initialNode = newPuzzleNode pieces (emptyPuzzle fitPiece)
    stopLooking node = any not (map (\r -> r node) rules)

islandFitsAPiece :: [[Piece]] -> S.Set Pos -> Bool
islandFitsAPiece pieces island = any fits pieces
  where
    fits piece = hasChildren (node piece)
    s          = emptyPuzzle (Piece ' ' island)
    node p     = PuzzleNode [p] s

hasChildren :: PuzzleNode -> Bool
hasChildren node = not $ null $ generateChildren node

