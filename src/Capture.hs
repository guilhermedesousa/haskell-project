module Capture where

import Data.Maybe (catMaybes, isJust)
import Data.List (intercalate)
import Data.Char (digitToInt, isDigit)
import Piece
import Player
import Utils
import Board

type CapturedPieces = ([Maybe Piece], [Maybe Piece])

addCapturedPiece :: CapturedPieces -> Maybe Piece -> CapturedPieces
addCapturedPiece capturedPieces piece = case piece of
    Nothing    -> capturedPieces
    Just piece -> do
        let player    = getPlayer piece
        let pieceType = getType piece
        case player of
            A -> do
                let newPiece = Just (Piece pieceType B False)
                (fst capturedPieces, addToPlayerList (snd capturedPieces) newPiece)
            B -> do
                let newPiece = Just (Piece pieceType A False)
                (addToPlayerList (fst capturedPieces) newPiece, snd capturedPieces)

removeCapturedPiece :: CapturedPieces -> Int -> Piece -> CapturedPieces
removeCapturedPiece capturedPieces i piece = case (getPlayer piece) of
    A -> (removeFromPlayerList (fst capturedPieces) (i - 1), snd capturedPieces)
    B -> (fst capturedPieces, removeFromPlayerList (snd capturedPieces) (i - 1))

addToPlayerList :: [Maybe Piece] -> Maybe Piece -> [Maybe Piece]
addToPlayerList xs capturedPiece = xs ++ [capturedPiece]

removeFromPlayerList :: [Maybe Piece] -> Int -> [Maybe Piece]
removeFromPlayerList xs i = take i xs ++ drop (i + 1) xs

indexCapturedPieces :: [Piece] -> [String]
indexCapturedPieces pieces = zipWith showIndexedPiece pieces [1..]
    where
        showIndexedPiece piece n = show n ++ ": " ++ pieceToString piece

printCapturedPieces :: [Maybe Piece] -> IO ()
printCapturedPieces pieces = putStrLn $ intercalate ", " (indexCapturedPieces (catMaybes pieces))

parsePieceChoice :: Int -> String -> Maybe Int
parsePieceChoice countCaptured playerChoice = case (strToInt playerChoice) of
    Just x -> if (x < 1 || x > countCaptured) then Nothing else Just x
    Nothing -> Nothing

validReplacement :: Maybe Piece -> Position -> Board -> Maybe Bool
validReplacement pieceToReplace (destRow, destCol) board = case (pieceToReplace) of
    Just piece -> do
        case (getType piece) of
            Peao -> do
                -- Verifica se há outro peão não promovido na mesma coluna
                let colHasUnpromotedPawn = any (\(row, Just mPiece) -> (getType mPiece) == Peao && not (isPromoted mPiece)) $
                                           filter (\(row, mPiece) -> isJust mPiece) $
                                           map (\row -> (row, board !! row !! destCol)) [0..8] -- (Int, (Int, Cell))

                let player = getPlayer piece
                -- Verifica se a posição destino está na última linha
                let isLastRow = if player == A then destRow == 8 else destRow == 0
                if colHasUnpromotedPawn || isLastRow
                    then Just False
                    else Just True
            _ -> Nothing
    _ -> Nothing