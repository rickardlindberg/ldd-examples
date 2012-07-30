import Lang.DSL
import Lang.PuzzleState
import Lang.SimpleSolver

main = do
  let square = parsePiece '#' [ "**"
                              , "**"
                              ]
  let stick  = parsePiece 'I' [ "*"
                              , "*"
                              ]
  let board  = parsePiece '.' [ "***"
                              , "***"
                              , "** "
                              ]
  putStrLn (printSolutions (findSolutions [square, stick] board))

