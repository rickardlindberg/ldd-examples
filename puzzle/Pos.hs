module Pos where

data Pos = Pos
    { x :: Int
    , y :: Int
    } deriving (Ord, Eq)

addPos :: Pos -> Pos -> Pos
addPos (Pos x1 y1) (Pos x2 y2) = Pos (x1+x2) (y1+y2)

posFlip :: Pos -> Pos
posFlip (Pos x y) = Pos (-1*x) y

posAdd :: Pos -> Pos -> Pos
posAdd (Pos x1 y1) (Pos x2 y2) = Pos (x1+x2) (y1+y2)

posRotate45 :: Pos -> Pos
posRotate45 (Pos x y) = Pos (-y) x

