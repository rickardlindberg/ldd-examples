module Piece where

import qualified Data.Set as S
import Pos

data Piece = Piece
    { symbol    :: Char
    , positions :: S.Set Pos
    }

