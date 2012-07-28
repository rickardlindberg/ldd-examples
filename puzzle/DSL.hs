module DSL where

import Data.Maybe
import qualified Data.Set as S
import Piece
import Pos

parsePiece :: Char -> [String] -> Piece

parsePiece symbol lines = Piece symbol (S.fromList (linesToPos lines))
    where
        linesToPos lines  = concatMap (\(y, line) -> lineToPos y line) (zip [0..] lines)
        lineToPos y line  = mapMaybe (\(x, char) -> charToPos y x char) (zip [0..] line)
        charToPos y x '*' = Just (Pos x y)
        charToPos _ _ _   = Nothing

