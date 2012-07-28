import Lang.DSL
import Lang.PuzzleState
import Lang.SimpleSolver

main = do
    let square = parsePiece '#' [ "**"
                                , "**"
                                ]
        stick  = parsePiece 'I' [ "*"
                                , "*"
                                ]
        board  = parsePiece '.' [ "***"
                                , "***"
                                , "** "
                                ]
    putStrLn (printSolutions (findSolutions [square, stick] board))

