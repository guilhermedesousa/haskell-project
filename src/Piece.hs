module Piece where

import Player

data PieceType = Peao | Lanca | Cavalo | General_Prata | General_Ouro | Bispo | Torre | Rei
    deriving (Eq, Show)

data Piece = Piece { getType    :: PieceType,
                     getPlayer  :: Player,
                     isPromoted :: Bool
                   } deriving (Eq, Show)