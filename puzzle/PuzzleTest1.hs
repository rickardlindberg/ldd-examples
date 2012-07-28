import DSL
import Pos
import Piece
import PuzzleState

main = do
    let square = parsePiece '#' [ "**"
                                , "**"
                                ]
    let stick  = parsePiece 'I' [ "*"
                                , "*"
                                ]
    let board  = parsePiece '.' [ "****"
                                , "****"
                                , "**"
                                ]
    let s1        = emptyPuzzle board
    let (Just s2) = putPiece (movePiece (Pos 0 0) square) s1
    let (Just s3) = putPiece (movePiece (Pos 2 0) stick) s2
    putStrLn $ printPuzzleState s3

