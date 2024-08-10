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

greenColor, yellowColor, resetColor :: String
greenColor = "\x1b[92m"  -- Verde
yellowColor = "\x1b[33m" -- Amarelo
resetColor = "\x1b[0m"   -- Reseta para cor padrão

printPiece :: Piece -> String
printPiece (Piece pieceType player) = case player of
    Branco -> case pieceType of
        Peao          -> yellowColor ++ "PB" ++ resetColor
        Lanca         -> yellowColor ++ "LB" ++ resetColor
        Cavalo        -> yellowColor ++ "CB" ++ resetColor
        General_Prata -> yellowColor ++ "SB" ++ resetColor
        General_Ouro  -> yellowColor ++ "GB" ++ resetColor
        Bispo         -> yellowColor ++ "BB" ++ resetColor
        Torre         -> yellowColor ++ "TB" ++ resetColor
        Rei           -> yellowColor ++ "RB" ++ resetColor
    Preto -> case pieceType of
        Peao          -> greenColor ++ "PP" ++ resetColor
        Lanca         -> greenColor ++ "LP" ++ resetColor
        Cavalo        -> greenColor ++ "CP" ++ resetColor
        General_Prata -> greenColor ++ "SP" ++ resetColor
        General_Ouro  -> greenColor ++ "GP" ++ resetColor
        Bispo         -> greenColor ++ "BP" ++ resetColor
        Torre         -> greenColor ++ "TP" ++ resetColor
        Rei           -> greenColor ++ "RP" ++ resetColor

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