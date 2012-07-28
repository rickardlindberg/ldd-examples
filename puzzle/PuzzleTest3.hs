import DSL
import SolverCommon
import PruningSolver
import PuzzleState
import qualified Data.Set as S

pentominoesPieces =
    [ parsePiece '+' [ " * "
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

pentominoesBoard =
    parsePiece '.' [ "**********"
                   , "**********"
                   , "**********"
                   , "**********"
                   , "**********"
                   , "**********"
                   ]

main = putStrLn (printSolutions pentominoesSolutions)

pentominoesSolutions = findSolutionsBetter pentominoesPieces pentominoesBoard pentominoesRules

pentominoesRules = [ allRules ]
allRules node =
    let is = islands $ puzzleState node
    in islandsMod5 is && islandFitsPiece node is
islandsMod5 is = all (\s -> S.size s `mod` 5 == 0) is
islandFitsPiece node is = all (islandFitsAPiece (piecesLeft node)) is
allPiecesFit node = all (\children -> hasChildren (node { piecesLeft = [children] })) (piecesLeft node)

