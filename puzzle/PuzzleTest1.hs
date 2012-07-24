import DSL
import Pos
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
    let (Just s2) = putPiece square (Pos 0 0) s1
    let (Just s3) = putPiece stick (Pos 2 0) s2
    putStrLn $ printPuzzleState s3

