import DSL
import Board

main = do
    let p1 = parsePiece '+' [ " * "
                            , "***"
                            , " * "
                            ]
    let p2 = parsePiece '#' [ "  ***"
                            , "    *"
                            ]
    let board = parsePiece '#' [ "*****"
                               , "*****"
                               , "*****"
                               ]
    putStrLn $ printBoard $ Board [] board [p1, p2]

