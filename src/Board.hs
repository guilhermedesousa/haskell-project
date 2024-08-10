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
    createThirdRow A,
    createSecondRow A,
    createFirstRow A,
    createEmptyRow,
    createEmptyRow,
    createEmptyRow,
    createFirstRow B,
    createSecondRow B,
    createThirdRow B]

greenColor, yellowColor, blueColor, whiteColor, resetColor :: String
greenColor  = "\x1b[92m" -- Verde
yellowColor = "\x1b[33m" -- Amarelo
blueColor   = "\x1b[34m" -- Azul
whiteColor  = "\x1b[37m" -- Branco
resetColor  = "\x1b[0m"  -- Reseta para cor padrão

printPiece :: Piece -> String
printPiece (Piece pieceType player) = case player of
    A -> case pieceType of
        Peao          -> yellowColor ++ "P" ++ resetColor
        Lanca         -> yellowColor ++ "L" ++ resetColor
        Cavalo        -> yellowColor ++ "C" ++ resetColor
        General_Prata -> yellowColor ++ "S" ++ resetColor
        General_Ouro  -> yellowColor ++ "G" ++ resetColor
        Bispo         -> yellowColor ++ "B" ++ resetColor
        Torre         -> yellowColor ++ "T" ++ resetColor
        Rei           -> yellowColor ++ "R" ++ resetColor
    B -> case pieceType of
        Peao          -> blueColor ++ "P" ++ resetColor
        Lanca         -> blueColor ++ "L" ++ resetColor
        Cavalo        -> blueColor ++ "C" ++ resetColor
        General_Prata -> blueColor ++ "S" ++ resetColor
        General_Ouro  -> blueColor ++ "G" ++ resetColor
        Bispo         -> blueColor ++ "B" ++ resetColor
        Torre         -> blueColor ++ "T" ++ resetColor
        Rei           -> blueColor ++ "R" ++ resetColor

printCell :: Cell -> String
printCell Nothing      = "   "
printCell (Just piece) = " " ++ printPiece piece ++ " "

printBoard :: Board -> IO ()
printBoard board = do
    putStrLn " a | b | c | d | e | f | g | h | i "
    putStrLn "-----------------------------------"
    putStrLn (unlines (zipWith showRow board [1..9]))
    where
        showRow row n = intercalate "|" (map printCell row) ++ "| " ++ show n