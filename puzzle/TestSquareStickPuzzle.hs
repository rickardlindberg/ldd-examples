import Lang.DSL
import Lang.PuzzleState
import Lang.SimpleSolver

squareStickSolutions :: String
squareStickSolutions =
    let square = parsePiece '#' [ "**"
                                , "**"
                                ]
        stick  = parsePiece 'I' [ "*"
                                , "*"
                                ]
        board  = parsePiece '.' [ "***"
                                , "***"
                                ]
    in  printSolutions (findSolutions [square, stick] board)

main = putStrLn squareStickSolutions

