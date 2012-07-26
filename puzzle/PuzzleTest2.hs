import DSL
import PuzzleState
import Solver

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
    putStrLn $ printSolutions (findSolutions [square, stick] board)

