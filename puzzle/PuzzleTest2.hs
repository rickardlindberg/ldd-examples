import DSL
import PuzzleState
import Solver

main = do
    let square = parsePiece '#' [ "**"
                                , "**"
                                ]
    let stick  = parsePiece 'I' [ "*"
                                , "*"
                                ]
    let board  = parsePiece '.' [ "***"
                                , "***"
                                , "***"
                                ]
    putStrLn $ printPuzzleStates (findSolutions [square, stick] board)

