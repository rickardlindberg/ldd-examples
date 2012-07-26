import DSL
import PuzzleState
import Solver

main = do
    let pieces = [ parsePiece '+' [ " * "
                                  , "***"
                                  , " * "
                                  ]
                 , parsePiece 'I' [ "*****"
                                  ]
                 , parsePiece 'c' [ "***"
                                  , "* *"
                                  ]
                 , parsePiece 'v' [ "***"
                                  , "*  "
                                  , "*  "
                                  ]
                 , parsePiece 'T' [ "***"
                                  , " * "
                                  , " * "
                                  ]
                 , parsePiece 'w' [ "*  "
                                  , "** "
                                  , " **"
                                  ]
                 , parsePiece 'L' [ "****"
                                  , "   *"
                                  ]
                 , parsePiece '*' [ "****"
                                  , "  * "
                                  ]
                 , parsePiece 'n' [ " ***"
                                  , "**  "
                                  ]
                 , parsePiece '#' [ "***"
                                  , "** "
                                  ]
                 , parsePiece 's' [ " **"
                                  , " * "
                                  , "** "
                                  ]
                 , parsePiece 'z' [ " * "
                                  , " **"
                                  , "** "
                                  ]
                 ]
    let board    = parsePiece '.' [ "**********"
                                  , "**********"
                                  , "**********"
                                  , "**********"
                                  , "**********"
                                  , "**********"
                                  ]
    putStrLn $ printSolutions (findSolutions pieces board)

