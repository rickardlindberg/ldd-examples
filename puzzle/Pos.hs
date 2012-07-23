module Pos where

data Pos = Pos
    { x :: Int
    , y :: Int
    } deriving (Ord, Eq)

