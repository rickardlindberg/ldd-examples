import DSL
import PuzzleState
import Solver

main = do
    let p1     = parsePiece 'I' [ "*****"
                                ]
    let p2     = parsePiece 'L' [ "****"
                                , "   *"
                                ]
    let p3     = parsePiece '*' [ "****"
                                , "  * "
                                ]
    let p4     = parsePiece 'n' [ " ***"
                                , "**  "
                                ]
    let p5     = parsePiece 'c' [ "***"
                                , "* *"
                                ]
    let p6     = parsePiece '#' [ "***"
                                , "** "
                                ]
    let p7     = parsePiece 'T' [ "***"
                                , " * "
                                , " * "
                                ]
    let p8     = parsePiece 'v' [ "***"
                                , "*  "
                                , "*  "
                                ]
    let p9     = parsePiece 's' [ " **"
                                , " * "
                                , "** "
                                ]
    let p10    = parsePiece 'z' [ " * "
                                , " **"
                                , "** "
                                ]
    let p11    = parsePiece '+' [ " * "
                                , "***"
                                , " * "
                                ]
    let p12    = parsePiece 'w' [ "*  "
                                , "** "
                                , " **"
                                ]
    let board  = parsePiece '.' [ "**********"
                                , "**********"
                                , "**********"
                                , "**********"
                                , "**********"
                                , "**********"
                                ]
    let board  = parsePiece '.' [ "********"
                                , "********"
                                , "********"
                                , "***  ***"
                                , "***  ***"
                                , "********"
                                , "********"
                                , "********"
                                ]
    putStrLn $ printPuzzleStates (findSolutions [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10] board)

