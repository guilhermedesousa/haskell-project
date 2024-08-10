module Board where

import Data.List (intercalate)
import Player
import Piece

type Cell = Maybe Piece
type Board = [[Cell]]

createCell :: Player -> PieceType -> Cell
createCell player pieceType = Just (Piece pieceType player)

-- [lança, cavalo, general de prata, general de ouro, rei, general de ouro, general de prata, cavalo, lança]
createThirdRow :: Player -> [Cell]
createThirdRow player = [createCell player Lanca, createCell player Cavalo, createCell player General_Prata,
                         createCell player General_Ouro, createCell player Rei, createCell player General_Ouro,
                         createCell player General_Prata, createCell player Cavalo, createCell player Lanca]

-- [vazio, bispo, vazio, vazio, vazio, vazio, vazio, torre, vazio]
createSecondRow :: Player -> [Cell]
createSecondRow player = [Nothing, createCell player Bispo, Nothing,
                          Nothing, Nothing, Nothing,
                          Nothing, createCell player Torre, Nothing]

-- [peão, peão, peão, peão, peão, peão, peão, peão, peão]
createFirstRow :: Player -> [Cell]
createFirstRow player = [createCell player Peao, createCell player Peao, createCell player Peao,
                         createCell player Peao, createCell player Peao, createCell player Peao,
                         createCell player Peao, createCell player Peao, createCell player Peao]

createEmptyRow :: [Cell]
createEmptyRow = [Nothing, Nothing, Nothing,
                  Nothing, Nothing, Nothing,
                  Nothing, Nothing, Nothing]

createInitialBoard :: Board
createInitialBoard = [
    createThirdRow Branco,
    createSecondRow Branco,
    createFirstRow Branco,
    createEmptyRow,
    createEmptyRow,
    createEmptyRow,
    createFirstRow Preto,
    createSecondRow Preto,
    createThirdRow Preto]

printPiece :: Piece -> String
printPiece (Piece pieceType player) = case (player) of
    Branco -> case (pieceType) of
        Peao          -> "PB"
        Lanca         -> "LB"
        Cavalo        -> "CB"
        General_Prata -> "SB" -- S = Silver
        General_Ouro  -> "GB" -- G = Gold
        Bispo         -> "BB"
        Torre         -> "TB"
        Rei           -> "RB"
    Preto -> case (pieceType) of
        Peao          -> "PP"
        Lanca         -> "LP"
        Cavalo        -> "CP"
        General_Prata -> "SP" -- S = Silver
        General_Ouro  -> "GP" -- G = Gold
        Bispo         -> "BP"
        Torre         -> "TP"
        Rei           -> "RP"

printCell :: Cell -> String
printCell Nothing      = "    "
printCell (Just piece) = " " ++ printPiece piece ++ " "

printBoard :: Board -> IO ()
printBoard board = do
    putStrLn " a  | b  | c  | d  | e  | f  | g  | h  | i  "
    putStrLn "--------------------------------------------"
    putStrLn (unlines (zipWith showRow board [1..9]))
    where
        showRow row n = intercalate "|" (map printCell row) ++ "| " ++ show n