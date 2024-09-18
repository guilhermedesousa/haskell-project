module Player where

data Player = A | B
    deriving (Eq, Show)

opponent :: Player -> Player
opponent A = B
opponent B = A