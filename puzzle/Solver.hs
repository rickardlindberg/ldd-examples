module Solver where

import Piece
import Pos
import PuzzleState
import TreeSearch
import Data.Maybe

findSolutions :: [Piece] -> Piece -> [PuzzleState]

data PuzzleNode = PuzzleNode
    { piecesLeft  :: [Piece]
    , puzzleState :: PuzzleState
    }

instance TreeNode PuzzleNode where
    generateChildren node = generatePuzzleNodeChildren node
    stopLooking      node = False
    isSolution       node = null (piecesLeft node)

generatePuzzleNodeChildren :: PuzzleNode -> [PuzzleNode]
generatePuzzleNodeChildren (PuzzleNode []     puzzleState) = []
generatePuzzleNodeChildren (PuzzleNode (x:xs) puzzleState) =
    let combinations = getCombinations x
        foo          = concatMap (getInner puzzleState) combinations
    in  map (PuzzleNode xs) foo

getInner :: PuzzleState -> Piece -> [PuzzleState]
getInner state piece = mapMaybe (\pos -> putPiece piece pos state) positions
    where
        positions = [Pos x y | x <- [0..maxX], y <- [0..maxY]]
        maxX      = pieceWidth  (fitPiece state) - pieceWidth  piece
        maxY      = pieceHeight (fitPiece state) - pieceHeight piece

findSolutions pieces fitPiece = map puzzleState (search (PuzzleNode pieces (emptyPuzzle fitPiece)))

